{-# LANGUAGE ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE InterruptibleFFI #-}
#endif

module System.Win32.FileNotify
       ( Handle
       , Action(..)
       , getWatchHandle
       , readDirectoryChanges
       ) where

import System.Win32.File
import System.Win32.Types

import Foreign
import Foreign.C

import Data.Bits


#include <windows.h>

type Handle = HANDLE

getWatchHandle :: FilePath -> IO Handle
getWatchHandle dir =
    createFile dir
        fILE_LIST_DIRECTORY -- Access mode
        (fILE_SHARE_READ .|. fILE_SHARE_WRITE) -- Share mode
        Nothing -- security attributes
        oPEN_EXISTING -- Create mode, we want to look at an existing directory
        fILE_FLAG_BACKUP_SEMANTICS -- File attribute, nb NOT using OVERLAPPED since we work synchronously
        Nothing -- No template file


readDirectoryChanges :: Handle -> Bool -> FileNotificationFlag -> IO [(Action, String)]
readDirectoryChanges h wst mask = do
  let maxBuf = 16384
  allocaBytes maxBuf $ \buffer -> do
    alloca $ \bret -> do
      readDirectoryChangesW h buffer (toEnum maxBuf) wst mask bret
      readChanges buffer

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
-- there are many more cases but I only need this one.
#endif

type FileAction = DWORD

#{enum FileAction,
 , fILE_ACTION_ADDED            = FILE_ACTION_ADDED
 , fILE_ACTION_REMOVED          = FILE_ACTION_REMOVED
 , fILE_ACTION_MODIFIED         = FILE_ACTION_MODIFIED
 , fILE_ACTION_RENAMED_OLD_NAME = FILE_ACTION_RENAMED_OLD_NAME
 , fILE_ACTION_RENAMED_NEW_NAME = FILE_ACTION_RENAMED_NEW_NAME
 }

type WCHAR = Word16
-- This is a bit overkill for now, I'll only use nullFunPtr anyway,
-- but who knows, maybe someday I'll want asynchronous callbacks on the OS level.
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


readDirectoryChangesW :: Handle -> Ptr FILE_NOTIFY_INFORMATION -> DWORD -> BOOL -> FileNotificationFlag -> LPDWORD -> IO ()
readDirectoryChangesW h buf bufSize wst f br =
  failIfFalse_ "ReadDirectoryChangesW" $ c_ReadDirectoryChangesW h (castPtr buf) bufSize wst f br nullPtr nullFunPtr

{-
asynchReadDirectoryChangesW :: Handle -> Ptr FILE_NOTIFY_INFORMATION -> DWORD -> BOOL -> FileNotificationFlag
                                -> LPOVERLAPPED -> IO ()
asynchReadDirectoryChangesW h buf bufSize wst f over =
  failIfFalse_ "ReadDirectoryChangesW" $ c_ReadDirectoryChangesW h (castPtr buf) bufSize wst f nullPtr over nullFunPtr

cbReadDirectoryChangesW :: Handle -> Ptr FILE_NOTIFY_INFORMATION -> DWORD -> BOOL -> FileNotificationFlag
                                -> LPOVERLAPPED -> IO BOOL
cbReadDirectoryChanges
-}

-- The interruptible qualifier will keep threads listening for events from hanging blocking when killed
#if __GLASGOW_HASKELL__ >= 701
foreign import stdcall interruptible "windows.h ReadDirectoryChangesW"
#else
foreign import stdcall safe "windows.h ReadDirectoryChangesW"
#endif
  c_ReadDirectoryChangesW :: Handle -> LPVOID -> DWORD -> BOOL -> DWORD -> LPDWORD -> LPOVERLAPPED -> LPOVERLAPPED_COMPLETION_ROUTINE -> IO BOOL

{-
type CompletionRoutine :: (DWORD, DWORD, LPOVERLAPPED) -> IO ()
foreign import ccall "wrapper"
    mkCompletionRoutine :: CompletionRoutine -> IO (FunPtr CompletionRoutine)

type LPOVERLAPPED = Ptr OVERLAPPED
type LPOVERLAPPED_COMPLETION_ROUTINE = FunPtr CompletionRoutine

data OVERLAPPED = OVERLAPPED
    {
    }


-- In System.Win32.File, but missing a crucial case:
-- type FileNotificationFlag = DWORD
-}

-- See https://msdn.microsoft.com/en-us/library/windows/desktop/aa365465(v=vs.85).aspx
#{enum FileNotificationFlag,
 , fILE_NOTIFY_CHANGE_FILE_NAME = FILE_NOTIFY_CHANGE_FILE_NAME
 , fILE_NOTIFY_CHANGE_DIR_NAME = FILE_NOTIFY_CHANGE_DIR_NAME
 , fILE_NOTIFY_CHANGE_ATTRIBUTES = FILE_NOTIFY_CHANGE_ATTRIBUTES
 , fILE_NOTIFY_CHANGE_SIZE = FILE_NOTIFY_CHANGE_SIZE
 , fILE_NOTIFY_CHANGE_LAST_WRITE = FILE_NOTIFY_CHANGE_LAST_WRITE
 , fILE_NOTIFY_CHANGE_LAST_ACCESS = FILE_NOTIFY_CHANGE_LAST_ACCESS
 , fILE_NOTIFY_CHANGE_CREATION = FILE_NOTIFY_CHANGE_CREATION
 , fILE_NOTIFY_CHANGE_SECURITY = FILE_NOTIFY_CHANGE_SECURITY
 }
