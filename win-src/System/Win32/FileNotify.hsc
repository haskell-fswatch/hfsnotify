{-# LANGUAGE ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE InterruptibleFFI #-}
#endif

{-# LANGUAGE LambdaCase #-}

module System.Win32.FileNotify (
  Handle
  , Action(..)
  , DirectoryWatch
  , directoryWatchHandle
  , openDirectoryWatch
  , armDirectoryWatch
  , awaitDirectoryWatch
  , closeDirectoryWatch
  , freeDirectoryWatch
  , eRROR_INVALID_HANDLE
  , eRROR_OPERATION_ABORTED
  ) where

import Data.Char (isSpace)
import Data.Function (on)
import Data.IORef
import Data.Ord (comparing)
import Foreign ((.|.), Ptr, FunPtr, alloca, callocBytes, castPtr, fillBytes, free, mallocBytes, nullFunPtr, peek, peekByteOff, plusPtr, pokeByteOff)
import Foreign.C (peekCWStringLen)
import Numeric (showHex)
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

-- | A directory opened for change notifications, along with the storage a read in flight writes
-- into.
--
-- Reads are overlapped (asynchronous), which lets us keep one in flight at all times. The
-- alternative -- a synchronous @ReadDirectoryChangesW@, as this module used to do -- loses every
-- change that happens while no call is outstanding, including all changes between opening the
-- handle and the first call.
data DirectoryWatch = DirectoryWatch {
  directoryWatchHandle :: Handle
  , dwWatchSubTree :: BOOL
  , dwMask :: FileNotificationFlag
  -- | Signalled when a read completes, including when it completes because closing the handle
  -- cancelled it. We wait on this rather than on the directory handle so that waiting doesn't
  -- depend on the handle still being open.
  , dwCompletionEvent :: Handle
  -- | The kernel writes into these while a read is in flight, so they have to be stable
  -- allocations rather than anything we hand out from the stack.
  , dwOverlapped :: Ptr ()
  , dwBuffers :: (Ptr FILE_NOTIFY_INFORMATION, Ptr FILE_NOTIFY_INFORMATION)
  -- | Which of 'dwBuffers' the read in flight is filling. We decode one buffer while the next
  -- read fills the other.
  , dwFillingBuffer :: IORef Int
  -- | Set when re-arming fails, so the failure can be reported on the next
  -- 'awaitDirectoryWatch' rather than losing the changes we had already decoded.
  , dwArmError :: IORef (Maybe (ErrCode, String))
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

-- | Open a directory for change notifications. Nothing is reported until 'armDirectoryWatch' has
-- put the first read in flight.
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
  buffers <- (,) <$> mallocBytes bufferSize <*> mallocBytes bufferSize
  fillingBuffer <- newIORef 0
  armError <- newIORef Nothing

  return $ DirectoryWatch {
    directoryWatchHandle = watchHandle
    , dwWatchSubTree = watchSubTree
    , dwMask = mask
    , dwCompletionEvent = completionEvent
    , dwOverlapped = overlapped
    , dwBuffers = buffers
    , dwFillingBuffer = fillingBuffer
    , dwArmError = armError
    }

-- | Start a read. Returns once the read is in flight, so changes made after this returns are
-- recorded by the OS even if nothing is waiting on them yet.
armDirectoryWatch :: DirectoryWatch -> IO (Either (ErrCode, String) ())
armDirectoryWatch dw = do
  buffer <- fillingBufferOf dw

  -- The OVERLAPPED struct has to start out zeroed apart from the event to signal on completion
  _ <- c_ResetEvent (dwCompletionEvent dw)
  fillBytes (dwOverlapped dw) 0 (#size OVERLAPPED)
  (#poke OVERLAPPED, hEvent) (dwOverlapped dw) (dwCompletionEvent dw)

  c_ReadDirectoryChangesW (directoryWatchHandle dw) (castPtr buffer) (toEnum bufferSize)
      (dwWatchSubTree dw) (dwMask dw) nullPtr (castPtr (dwOverlapped dw)) nullFunPtr >>= \case
    -- Completed synchronously, which is allowed and fine: the results are in the buffer and
    -- GetOverlappedResult will return them straight away.
    True -> return $ Right ()
    False -> getLastError >>= \case
      -- The normal case: the read is in flight
      err | err == eRROR_IO_PENDING -> return $ Right ()
          | otherwise -> Left <$> lastErrorMessage err

-- | Wait for the read in flight to complete, immediately put the next read in flight, and decode
-- the completed one. Re-arming before decoding is what keeps changes from being missed while we
-- work.
awaitDirectoryWatch :: DirectoryWatch -> IO (Either (ErrCode, String) [(Action, String)])
awaitDirectoryWatch dw = readIORef (dwArmError dw) >>= \case
  Just err -> return $ Left err
  Nothing -> alloca $ \bytesReturnedPtr ->
    c_GetOverlappedResult (directoryWatchHandle dw) (castPtr (dwOverlapped dw)) bytesReturnedPtr True >>= \case
      False -> Left <$> (getLastError >>= lastErrorMessage)
      True -> do
        bytesReturned <- peek bytesReturnedPtr
        completedBuffer <- fillingBufferOf dw
        modifyIORef' (dwFillingBuffer dw) (\i -> 1 - i)

        armDirectoryWatch dw >>= \case
          Left err -> writeIORef (dwArmError dw) (Just err)
          Right () -> return ()

        if bytesReturned == 0
          -- A completion with no bytes means the buffer overflowed and those changes are gone.
          -- There's nothing to decode; ideally this would surface to the user as a "rescan this
          -- directory" event.
          then return $ Right []
          else Right <$> readChanges completedBuffer

-- | Close the directory handle, which cancels the read in flight. A concurrent
-- 'awaitDirectoryWatch' then fails with 'eRROR_OPERATION_ABORTED' (or 'eRROR_INVALID_HANDLE' if
-- it hadn't started waiting yet), which is the signal for the reader to stop.
closeDirectoryWatch :: DirectoryWatch -> IO ()
closeDirectoryWatch = closeHandle . directoryWatchHandle

-- | Release the buffers and the completion event. Only safe once no read can still be in flight,
-- i.e. after 'awaitDirectoryWatch' has returned an error, since until then the kernel may write
-- into them.
freeDirectoryWatch :: DirectoryWatch -> IO ()
freeDirectoryWatch dw = do
  closeHandle (dwCompletionEvent dw)
  free (dwOverlapped dw)
  free (fst (dwBuffers dw))
  free (snd (dwBuffers dw))

fillingBufferOf :: DirectoryWatch -> IO (Ptr FILE_NOTIFY_INFORMATION)
fillingBufferOf dw = readIORef (dwFillingBuffer dw) >>= \case
  0 -> return $ fst (dwBuffers dw)
  _ -> return $ snd (dwBuffers dw)

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

eRROR_INVALID_HANDLE :: ErrCode
eRROR_INVALID_HANDLE = (#const ERROR_INVALID_HANDLE)

eRROR_IO_PENDING :: ErrCode
eRROR_IO_PENDING = (#const ERROR_IO_PENDING)

eRROR_OPERATION_ABORTED :: ErrCode
eRROR_OPERATION_ABORTED = (#const ERROR_OPERATION_ABORTED)

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
