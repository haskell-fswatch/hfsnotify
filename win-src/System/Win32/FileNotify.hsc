{-# LANGUAGE ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE InterruptibleFFI #-}
#endif

{-# LANGUAGE LambdaCase #-}

module System.Win32.FileNotify (
  Handle
  , Action(..)
  , DirectoryWatch
  , openDirectoryWatch
  , armDirectoryWatch
  , awaitDirectoryWatch
  , directoryWatchStopping
  , signalReaderFinished
  , stopDirectoryWatch
  , abandonDirectoryWatch
  ) where

import Control.Concurrent.MVar
import Control.Monad (unless, void)
import Data.Char (isSpace)
import Data.Function (on)
import Data.IORef
import Data.Ord (comparing)
import Foreign ((.|.), Ptr, FunPtr, alloca, callocBytes, castPtr, fillBytes, free, mallocBytes, nullFunPtr, peek, peekByteOff, plusPtr, pokeByteOff)
import Foreign.C (peekCWStringLen)
import Numeric (showHex)
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
  , failIfNull
  , getErrorMessage
  , getLastError
  , localFree
  , nullPtr
  )
import System.Win32.Types (peekTString)


#include <windows.h>

type Handle = HANDLE

-- | A directory open for change notifications, along with the storage a read in flight writes
-- into.
--
-- Reads are overlapped (asynchronous) for one reason: it lets the first read be issued by the
-- thread that sets the watch up, before anyone can act on the directory. Windows records nothing
-- for a handle until a read has been issued, so a change made before that point is lost with
-- nothing to recover it from -- waiting longer doesn't help, the change was never recorded.
data DirectoryWatch = DirectoryWatch {
  directoryWatchHandle :: Handle
  , dwWatchSubTree :: BOOL
  , dwMask :: FileNotificationFlag
  -- | Signalled by the OS when a read completes, including when it completes because the read was
  -- cancelled. We wait on this rather than on the directory handle, so that waiting doesn't
  -- depend on the handle still being open.
  , dwCompletionEvent :: Handle
  -- | The OS writes into these while a read is in flight, so they can't be stack allocated, and
  -- they can only be released once we know no read is in flight. See 'stopDirectoryWatch'.
  , dwOverlapped :: Ptr ()
  , dwBuffer :: Ptr FILE_NOTIFY_INFORMATION
  -- | Set by 'stopDirectoryWatch'. Doubles as the "cleanup has begun" flag, so stopping twice is
  -- harmless.
  , dwStopping :: IORef Bool
  -- | Filled by the reader when it stops and no read is in flight.
  , dwReaderFinished :: MVar ()
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

-- | How long each attempt in 'stopDirectoryWatch' waits for the reader to report back, and how
-- many attempts it makes before giving up on releasing its memory.
cancelAttemptTimeout :: Int
cancelAttemptTimeout = 500000

cancelAttempts :: Int
cancelAttempts = 10

-- | Open a directory for change notifications. Nothing is recorded until 'armDirectoryWatch' has
-- issued the first read.
openDirectoryWatch :: FilePath -> Bool -> FileNotificationFlag -> IO DirectoryWatch
openDirectoryWatch dir watchSubTree mask = do
  watchHandle <- createFile dir
    fILE_LIST_DIRECTORY -- Access mode
    (fILE_SHARE_READ .|. fILE_SHARE_WRITE) -- Share mode
    Nothing -- security attributes
    oPEN_EXISTING -- Create mode, we want to look at an existing directory
    (fILE_FLAG_BACKUP_SEMANTICS .|. fILE_FLAG_OVERLAPPED) -- Directory handle, asynchronous reads
    Nothing -- No template file

  -- Manual reset, initially unsignalled; we reset it ourselves before each read
  completionEvent <- failIfNull "CreateEvent" $ c_CreateEventW nullPtr True False nullPtr

  overlapped <- callocBytes (#size OVERLAPPED)
  buffer <- mallocBytes bufferSize
  stopping <- newIORef False
  readerFinished <- newEmptyMVar

  return $ DirectoryWatch {
    directoryWatchHandle = watchHandle
    , dwWatchSubTree = watchSubTree
    , dwMask = mask
    , dwCompletionEvent = completionEvent
    , dwOverlapped = overlapped
    , dwBuffer = buffer
    , dwStopping = stopping
    , dwReaderFinished = readerFinished
    }

-- | Issue a read. Returns once the read is in flight, so changes made after this returns are
-- recorded by the OS even if nothing is waiting on them yet.
--
-- Changes that happen between reads aren't lost: the OS keeps buffering them against the handle
-- and hands them over on the next read.
armDirectoryWatch :: DirectoryWatch -> IO (Either (ErrCode, String) ())
armDirectoryWatch dw = do
  -- The OVERLAPPED has to start out zeroed apart from the event to signal on completion
  _ <- c_ResetEvent (dwCompletionEvent dw)
  fillBytes (dwOverlapped dw) 0 (#size OVERLAPPED)
  (#poke OVERLAPPED, hEvent) (dwOverlapped dw) (dwCompletionEvent dw)

  c_ReadDirectoryChangesW (directoryWatchHandle dw) (castPtr (dwBuffer dw)) (toEnum bufferSize)
      (dwWatchSubTree dw) (dwMask dw) nullPtr (castPtr (dwOverlapped dw)) nullFunPtr >>= \case
    -- Completed without having to wait, which is allowed and fine: the results are in the buffer
    -- and 'awaitDirectoryWatch' will hand them over straight away.
    True -> return $ Right ()
    False -> getLastError >>= \case
      -- The normal case: the read is in flight
      err | err == eRROR_IO_PENDING -> return $ Right ()
          | otherwise -> Left <$> lastErrorMessage err

-- | Wait for the read in flight to complete and decode it. Also returns when the read is
-- cancelled by 'stopDirectoryWatch', which is how the reader learns to stop.
awaitDirectoryWatch :: DirectoryWatch -> IO (Either (ErrCode, String) [(Action, String)])
awaitDirectoryWatch dw = alloca $ \bytesReturnedPtr ->
  c_GetOverlappedResult (directoryWatchHandle dw) (castPtr (dwOverlapped dw)) bytesReturnedPtr True >>= \case
    False -> Left <$> (getLastError >>= lastErrorMessage)
    True -> do
      bytesReturned <- peek bytesReturnedPtr
      if bytesReturned == 0
        -- A completion with no bytes means the OS buffer overflowed and those changes are gone.
        -- There's nothing to decode; ideally this would reach the user as a "rescan this
        -- directory" event.
        then return $ Right []
        else Right <$> readChanges (dwBuffer dw)

-- | Whether the watch is being stopped, which tells the reader to stop rather than issue another
-- read.
directoryWatchStopping :: DirectoryWatch -> IO Bool
directoryWatchStopping = readIORef . dwStopping

-- | Called by the reader once it has stopped and no read is in flight. This is what lets
-- 'stopDirectoryWatch' release the watch's memory.
signalReaderFinished :: DirectoryWatch -> IO ()
signalReaderFinished dw = void $ tryPutMVar (dwReaderFinished dw) ()

-- | Cancel the read in flight and release the watch once its reader confirms it has stopped.
--
-- Cancelling, rather than just closing the handle, is what makes this safe. A cancelled read
-- still completes, so the reader's wait returns and we learn the OS has finished with the buffer.
-- Closing the handle cancels asynchronously and gives no such signal, leaving no moment at which
-- freeing the buffer is known to be safe.
stopDirectoryWatch :: DirectoryWatch -> IO ()
stopDirectoryWatch dw = do
  alreadyStopping <- atomicModifyIORef' (dwStopping dw) $ \stopping -> (True, stopping)
  unless alreadyStopping $
    waitForReader cancelAttempts >>= \case
      Just () -> do
        closeHandle (directoryWatchHandle dw)
        closeHandle (dwCompletionEvent dw)
        freeDirectoryWatch dw
      Nothing ->
        -- The reader never reported back, so a read may still be in flight. Closing the directory
        -- handle unblocks it; leak the rest rather than hand the OS freed memory to write into.
        closeHandle (directoryWatchHandle dw)
  where
    -- We re-cancel on each attempt because the reader can issue a read in the instant between
    -- checking whether we're stopping and us cancelling, and that read needs cancelling too.
    waitForReader attemptsLeft
      | attemptsLeft <= (0 :: Int) = return Nothing
      | otherwise = do
          _ <- c_CancelIoEx (directoryWatchHandle dw) nullPtr
          timeout cancelAttemptTimeout (takeMVar (dwReaderFinished dw)) >>= \case
            Just () -> return $ Just ()
            Nothing -> waitForReader (attemptsLeft - 1)

-- | Release a watch whose reader was never started, so nothing can be in flight.
abandonDirectoryWatch :: DirectoryWatch -> IO ()
abandonDirectoryWatch dw = do
  closeHandle (directoryWatchHandle dw)
  closeHandle (dwCompletionEvent dw)
  freeDirectoryWatch dw

freeDirectoryWatch :: DirectoryWatch -> IO ()
freeDirectoryWatch dw = do
  free (dwOverlapped dw)
  free (dwBuffer dw)

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

-- | Extract the failure message, as done in https://hackage.haskell.org/package/Win32-2.14.0.0/docs/src/System.Win32.WindowsString.Types.html#errorWin
lastErrorMessage :: ErrCode -> IO (ErrCode, String)
lastErrorMessage err_code = do
  msg <- getErrorMessage err_code >>= \case
    x | x == nullPtr -> return $ "Error 0x" ++ Numeric.showHex err_code ""
    c_msg -> do
      msg <- peekTString c_msg
      -- We ignore failure of freeing c_msg, given we're already failing
      _ <- localFree c_msg
      return msg
  let msg' = reverse $ dropWhile isSpace $ reverse msg -- drop trailing \n
  return (err_code, msg')

-------------------------------------------------------------------
-- Low-level stuff that binds to notifications in the Win32 API

-- Defined in System.Win32.File, but with too few cases:
-- type AccessMode = UINT

#if !(MIN_VERSION_Win32(2,4,0))
#{enum AccessMode,
 , fILE_LIST_DIRECTORY = FILE_LIST_DIRECTORY
 }
-- there are many more cases but I only need this one.
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
