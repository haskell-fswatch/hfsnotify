{-# LANGUAGE ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE InterruptibleFFI #-}
#endif

{-# LANGUAGE LambdaCase #-}

module System.Win32.FileNotify (
  Handle
  , Action(..)
  , ReadChanges(..)
  , DirectoryWatch
  , openDirectoryWatch
  , directoryWatchOnError
  , startDirectoryWatch
  , armDirectoryWatch
  , awaitDirectoryWatch
  , directoryWatchStopping
  , stopDirectoryWatch
  ) where

import Control.Concurrent.Async (Async, asyncBound, waitCatch)
import Control.Concurrent.MVar
import Control.Exception (mask_, uninterruptibleMask_)
import Control.Exception.Safe (SomeException, bracketOnError, catch, onException, throwIO, toException, tryAny)
import Control.Monad (unless)
import Data.Function (on)
import Data.IORef
import Data.Ord (comparing)
import Foreign ((.|.), Ptr, FunPtr, alloca, callocBytes, castPtr, fillBytes, free, mallocBytes, nullFunPtr, peek, peekByteOff, plusPtr, pokeByteOff)
import Foreign.C (peekCWStringLen)
import System.Timeout (timeout)
import System.Win32.File (
  FileNotificationFlag
  , LPOVERLAPPED
  , closeHandle
  , createFile
  , oPEN_EXISTING
  , fILE_FLAG_BACKUP_SEMANTICS
  , fILE_FLAG_OVERLAPPED
  , fILE_LIST_DIRECTORY
  , fILE_SHARE_READ
  , fILE_SHARE_WRITE
  )
import System.Win32.Types (
  BOOL
  , DWORD
  , ErrCode
  , HANDLE
  , LPDWORD
  , LPVOID
  , errorWin
  , failIfNull
  , failWith
  , getLastError
  , nullPtr
  )


#include <windows.h>

type Handle = HANDLE

data DirectoryWatch = DirectoryWatch {
  directoryWatchHandle :: Handle
  , dwDirectory :: FilePath
  , dwOnError :: SomeException -> IO ()
  , dwWatchSubTree :: BOOL
  , dwMask :: FileNotificationFlag
  , dwCompletionEvent :: Handle
  , dwOverlapped :: Ptr ()
  , dwBuffer :: Ptr FILE_NOTIFY_INFORMATION
  , dwStopping :: IORef Bool
  , dwReader :: MVar (Async ())
  }

-- The handle identifies the watch; the rest is just its storage.
instance Eq DirectoryWatch where
  (==) = (==) `on` directoryWatchHandle
instance Ord DirectoryWatch where
  compare = comparing directoryWatchHandle
instance Show DirectoryWatch where
  show = show . directoryWatchHandle

bufferSize :: Int
bufferSize = 16384

-- | How long 'stopDirectoryWatch' waits per attempt, and how many attempts it makes.
cancelAttemptTimeout :: Int
cancelAttemptTimeout = 500000

cancelAttempts :: Int
cancelAttempts = 10

-- | Open a directory for change notifications. Nothing is recorded until 'armDirectoryWatch'.
openDirectoryWatch :: FilePath -> Bool -> FileNotificationFlag -> (SomeException -> IO ()) -> IO DirectoryWatch
openDirectoryWatch dir watchSubTree mask onError =
  bracketOnError openHandle closeHandleIgnoringExceptions $ \watchHandle ->
  bracketOnError openCompletionEvent closeHandleIgnoringExceptions $ \completionEvent ->
  bracketOnError (callocBytes (#size OVERLAPPED)) free $ \overlapped ->
  bracketOnError (mallocBytes bufferSize) free $ \buffer -> do
    stopping <- newIORef False
    reader <- newEmptyMVar

    return $ DirectoryWatch {
      directoryWatchHandle = watchHandle
      , dwDirectory = dir
      , dwOnError = onError
      , dwWatchSubTree = watchSubTree
      , dwMask = mask
      , dwCompletionEvent = completionEvent
      , dwOverlapped = overlapped
      , dwBuffer = buffer
      , dwStopping = stopping
      , dwReader = reader
      }

  where
    openHandle = createFile dir
      fILE_LIST_DIRECTORY -- Access mode
      (fILE_SHARE_READ .|. fILE_SHARE_WRITE) -- Share mode
      Nothing -- security attributes
      oPEN_EXISTING -- Create mode, we want to look at an existing directory
      (fILE_FLAG_BACKUP_SEMANTICS .|. fILE_FLAG_OVERLAPPED) -- Directory handle, asynchronous reads
      Nothing -- No template file

    -- Manual reset, initially unsignalled; we reset it ourselves before each read
    openCompletionEvent = failIfNull "CreateEvent" $ c_CreateEventW nullPtr True False nullPtr

-- | Start the watch's reader thread, making sure the watch is armed before
-- returning. Uses the same bound thread for arming and reading, as Windows
-- overlapped IO requires.
startDirectoryWatch :: DirectoryWatch -> IO () -> IO ()
startDirectoryWatch dw readerLoop = do
  armed <- newEmptyMVar

  -- Masked, so an interrupt can't leave a running reader unregistered, which
  -- teardown would read as "never started" and release the buffers under it.
  mask_ $ do
    reader <- asyncBound $ tryAny (armDirectoryWatch dw) >>= \case
      Left err -> putMVar armed (Left err)
      Right () -> putMVar armed (Right ()) >> readerLoop
    putMVar (dwReader dw) reader

  takeMVar armed >>= either throwIO return

-- | Once the first read has been accepted by the OS, all events are recorded.
armDirectoryWatch :: DirectoryWatch -> IO ()
armDirectoryWatch dw = do
  -- The OVERLAPPED has to start out zeroed apart from the event to signal on completion
  _ <- c_ResetEvent (dwCompletionEvent dw)
  fillBytes (dwOverlapped dw) 0 (#size OVERLAPPED)
  (#poke OVERLAPPED, hEvent) (dwOverlapped dw) (dwCompletionEvent dw)

  c_ReadDirectoryChangesW (directoryWatchHandle dw) (castPtr (dwBuffer dw)) (toEnum bufferSize)
      (dwWatchSubTree dw) (dwMask dw) nullPtr (castPtr (dwOverlapped dw)) nullFunPtr >>= \case
    -- Completed without waiting, which is fine: 'awaitDirectoryWatch' picks the results up.
    True -> return ()
    False -> getLastError >>= \case
      -- The normal case: the read is in flight
      err | err == eRROR_IO_PENDING -> return ()
          | otherwise -> failWith "ReadDirectoryChangesW" err

-- | What a completed read produced.
data ReadChanges =
  Changes [(Action, String)]
  -- | The OS buffer overflowed: those changes are gone and the directory should be rescanned.
  | Overflowed

awaitDirectoryWatch :: DirectoryWatch -> IO ReadChanges
awaitDirectoryWatch dw = alloca $ \bytesReturnedPtr ->
  c_GetOverlappedResult (directoryWatchHandle dw) (castPtr (dwOverlapped dw)) bytesReturnedPtr True >>= \case
    False -> errorWin "GetOverlappedResult"
    True -> do
      bytesReturned <- peek bytesReturnedPtr
      if bytesReturned == 0
        then return Overflowed
        else Changes <$> readChanges (dwBuffer dw)

directoryWatchOnError :: DirectoryWatch -> SomeException -> IO ()
directoryWatchOnError = dwOnError

directoryWatchStopping :: DirectoryWatch -> IO Bool
directoryWatchStopping = readIORef . dwStopping

-- | Cancel the read in flight, wait for the reader to stop, and release the watch.
stopDirectoryWatch :: DirectoryWatch -> IO ()
stopDirectoryWatch dw = do
  alreadyStopping <- atomicModifyIORef' (dwStopping dw) $ \stopping -> (True, stopping)
  unless alreadyStopping $ do
    readerStopped <- waitForReaderToStop cancelAttempts `onException` closeDirectory
    if readerStopped
      then releaseEverything
      else do
        closeDirectory
        dwOnError dw $ toException $ userError $
          "stopped watching "
          <> dwDirectory dw
          <> " but its reader is still running, so the OS still holds its buffers"

  where
    waitForReaderToStop attemptsLeft
      | attemptsLeft <= (0 :: Int) = return False
      | otherwise = tryReadMVar (dwReader dw) >>= \case
          Nothing -> return True  -- never started, so nothing can be in flight
          Just reader -> do
            _ <- c_CancelIoEx (directoryWatchHandle dw) nullPtr
            timeout cancelAttemptTimeout (waitCatch reader) >>= \case
              Just _ -> return True
              Nothing -> waitForReaderToStop (attemptsLeft - 1)

    -- Uninterruptible so an async exception can't leave this half done
    releaseEverything = uninterruptibleMask_ $ do
      closeHandleIgnoringExceptions (directoryWatchHandle dw)
      closeHandleIgnoringExceptions (dwCompletionEvent dw)
      free (dwOverlapped dw)
      free (dwBuffer dw)

    closeDirectory = uninterruptibleMask_ (closeHandleIgnoringExceptions (directoryWatchHandle dw))

closeHandleIgnoringExceptions :: Handle -> IO ()
closeHandleIgnoringExceptions h = closeHandle h `catch` \(_ :: SomeException) -> return ()

data Action = FileAdded | FileRemoved | FileModified | FileRenamedOld | FileRenamedNew
  deriving (Show, Read, Eq, Ord, Enum)

readChanges :: Ptr FILE_NOTIFY_INFORMATION -> IO [(Action, String)]
readChanges pfni = do
  fni <- peekFNI pfni
  let entry = (faToAction $ fniAction fni, fniFileName fni)
      nioff = fromEnum $ fniNextEntryOffset fni
  entries <- if nioff == 0 then return [] else readChanges $ pfni `plusPtr` nioff
  return $ entry:entries

faToAction :: FileAction -> Action
faToAction fa = toEnum $ fromEnum fa - 1

-------------------------------------------------------------------
-- Low-level stuff that binds to notifications in the Win32 API

-- Defined in System.Win32.File, but with too few cases:
-- type AccessMode = UINT

#if !(MIN_VERSION_Win32(2,4,0))
#{enum AccessMode,
 , fILE_LIST_DIRECTORY = FILE_LIST_DIRECTORY
 }
-- there are many more cases but we only need this one
#endif

eRROR_IO_PENDING :: ErrCode
eRROR_IO_PENDING = (#const ERROR_IO_PENDING)

type FileAction = DWORD

#{enum FileAction,
 , _fILE_ACTION_ADDED            = FILE_ACTION_ADDED
 , _fILE_ACTION_REMOVED          = FILE_ACTION_REMOVED
 , _fILE_ACTION_MODIFIED         = FILE_ACTION_MODIFIED
 , _fILE_ACTION_RENAMED_OLD_NAME = FILE_ACTION_RENAMED_OLD_NAME
 , _fILE_ACTION_RENAMED_NEW_NAME = FILE_ACTION_RENAMED_NEW_NAME
 }

-- type WCHAR = Word16

type LPOVERLAPPED_COMPLETION_ROUTINE = FunPtr ((DWORD, DWORD, LPOVERLAPPED) -> IO ())

data FILE_NOTIFY_INFORMATION = FILE_NOTIFY_INFORMATION
    { fniNextEntryOffset, fniAction :: DWORD
    , fniFileName :: String
    }

-- instance Storable FILE_NOTIFY_INFORMATION where
-- ... well, we can't write an instance since the struct is not of fix size,
-- so we'll have to do it the hard way, and not get anything for free. Sigh.

-- sizeOfFNI :: FILE_NOTIFY_INFORMATION -> Int
-- sizeOfFNI fni =  (#size FILE_NOTIFY_INFORMATION) + (#size WCHAR) * (length (fniFileName fni) - 1)

peekFNI :: Ptr FILE_NOTIFY_INFORMATION -> IO FILE_NOTIFY_INFORMATION
peekFNI buf = do
  neof <- (#peek FILE_NOTIFY_INFORMATION, NextEntryOffset) buf
  acti <- (#peek FILE_NOTIFY_INFORMATION, Action) buf
  fnle <- (#peek FILE_NOTIFY_INFORMATION, FileNameLength) buf
  fnam <- peekCWStringLen
            (buf `plusPtr` (#offset FILE_NOTIFY_INFORMATION, FileName), -- start of array
            fromEnum (fnle :: DWORD) `div` 2 ) -- fnle is the length in *bytes*, and a WCHAR is 2 bytes
  return $ FILE_NOTIFY_INFORMATION neof acti fnam

-- The interruptible qualifier will keep threads listening for events from hanging blocking when killed
#if __GLASGOW_HASKELL__ >= 701
foreign import stdcall interruptible "windows.h ReadDirectoryChangesW"
#else
foreign import stdcall safe "windows.h ReadDirectoryChangesW"
#endif
  c_ReadDirectoryChangesW :: Handle -> LPVOID -> DWORD -> BOOL -> DWORD -> LPDWORD -> LPOVERLAPPED -> LPOVERLAPPED_COMPLETION_ROUTINE -> IO BOOL

#if __GLASGOW_HASKELL__ >= 701
foreign import stdcall interruptible "windows.h GetOverlappedResult"
#else
foreign import stdcall safe "windows.h GetOverlappedResult"
#endif
  c_GetOverlappedResult :: Handle -> LPOVERLAPPED -> LPDWORD -> BOOL -> IO BOOL

foreign import stdcall unsafe "windows.h CancelIoEx"
  c_CancelIoEx :: Handle -> LPOVERLAPPED -> IO BOOL

foreign import stdcall unsafe "windows.h CreateEventW"
  c_CreateEventW :: Ptr () -> BOOL -> BOOL -> Ptr () -> IO Handle

foreign import stdcall unsafe "windows.h ResetEvent"
  c_ResetEvent :: Handle -> IO BOOL

-- See https://msdn.microsoft.com/en-us/library/windows/desktop/aa365465(v=vs.85).aspx
#{enum FileNotificationFlag,
 , _fILE_NOTIFY_CHANGE_FILE_NAME = FILE_NOTIFY_CHANGE_FILE_NAME
 , _fILE_NOTIFY_CHANGE_DIR_NAME = FILE_NOTIFY_CHANGE_DIR_NAME
 , _fILE_NOTIFY_CHANGE_ATTRIBUTES = FILE_NOTIFY_CHANGE_ATTRIBUTES
 , _fILE_NOTIFY_CHANGE_SIZE = FILE_NOTIFY_CHANGE_SIZE
 , _fILE_NOTIFY_CHANGE_LAST_WRITE = FILE_NOTIFY_CHANGE_LAST_WRITE
 , _fILE_NOTIFY_CHANGE_LAST_ACCESS = FILE_NOTIFY_CHANGE_LAST_ACCESS
 , _fILE_NOTIFY_CHANGE_CREATION = FILE_NOTIFY_CHANGE_CREATION
 , _fILE_NOTIFY_CHANGE_SECURITY = FILE_NOTIFY_CHANGE_SECURITY
 }
