{-# LANGUAGE ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE InterruptibleFFI #-}
#endif

{-# LANGUAGE LambdaCase #-}

module System.Win32.FileNotify (
  Handle
  , Action(..)
  , getWatchHandle
  , readDirectoryChanges
  ) where

import Data.Char (isSpace)
import Foreign ((.|.), Ptr, FunPtr, alloca, allocaBytes, castPtr, nullFunPtr, peekByteOff, plusPtr)
import Foreign.C (peekCWStringLen)
import GHC.IO.Encoding.Failure (CodingFailureMode(..))
import GHC.IO.Encoding.UTF16 (mkUTF16le)
import Numeric (showHex)
import System.OsString.Windows (decodeWith, encodeWith)
import System.Win32.File (
  FileNotificationFlag
  , LPOVERLAPPED
  , createFile
  , oPEN_EXISTING
  , fILE_FLAG_BACKUP_SEMANTICS
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
  , getErrorMessage
  , getLastError
  , localFree
  , nullPtr
  )
import System.Win32.Types (peekTString)


#include <windows.h>

type Handle = HANDLE

getWatchHandle :: FilePath -> IO Handle
getWatchHandle dir = createFile dir
  fILE_LIST_DIRECTORY -- Access mode
  (fILE_SHARE_READ .|. fILE_SHARE_WRITE) -- Share mode
  Nothing -- security attributes
  oPEN_EXISTING -- Create mode, we want to look at an existing directory
  fILE_FLAG_BACKUP_SEMANTICS -- File attribute, nb NOT using OVERLAPPED since we work synchronously
  Nothing -- No template file


readDirectoryChanges :: Handle -> Bool -> FileNotificationFlag -> IO (Either (ErrCode, String) [(Action, String)])
readDirectoryChanges h wst mask = do
  let maxBuf = 16384
  allocaBytes maxBuf $ \buffer -> do
    alloca $ \bret -> do
      readDirectoryChangesW h buffer (toEnum maxBuf) wst mask bret >>= \case
        Left err -> return $ Left err
        Right () -> Right <$> readChanges buffer

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
 , _fILE_ACTION_ADDED            = FILE_ACTION_ADDED
 , _fILE_ACTION_REMOVED          = FILE_ACTION_REMOVED
 , _fILE_ACTION_MODIFIED         = FILE_ACTION_MODIFIED
 , _fILE_ACTION_RENAMED_OLD_NAME = FILE_ACTION_RENAMED_OLD_NAME
 , _fILE_ACTION_RENAMED_NEW_NAME = FILE_ACTION_RENAMED_NEW_NAME
 }

-- type WCHAR = Word16

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


readDirectoryChangesW :: Handle -> Ptr FILE_NOTIFY_INFORMATION -> DWORD -> BOOL -> FileNotificationFlag -> LPDWORD -> IO (Either (ErrCode, String) ())
readDirectoryChangesW h buf bufSize wst f br =
  c_ReadDirectoryChangesW h (castPtr buf) bufSize wst f br nullPtr nullFunPtr >>= \case
    True -> return $ Right ()
    False -> do
      -- Extract the failure message, as done in https://hackage.haskell.org/package/Win32-2.14.0.0/docs/src/System.Win32.WindowsString.Types.html#errorWin
      err_code <- getLastError
      msg <- getErrorMessage err_code >>= \case
        x | x == nullFunPtr -> return $ "Error 0x" ++ Numeric.showHex err_code ""
          | otherwise -> do
              msg <- peekTString c_msg
              -- We ignore failure of freeing c_msg, given we're already failing
              _ <- localFree c_msg
              return msg
      let msg' = reverse $ dropWhile isSpace $ reverse msg -- drop trailing \n
      return $ Left (err_code, msg')

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
 , _fILE_NOTIFY_CHANGE_FILE_NAME = FILE_NOTIFY_CHANGE_FILE_NAME
 , _fILE_NOTIFY_CHANGE_DIR_NAME = FILE_NOTIFY_CHANGE_DIR_NAME
 , _fILE_NOTIFY_CHANGE_ATTRIBUTES = FILE_NOTIFY_CHANGE_ATTRIBUTES
 , _fILE_NOTIFY_CHANGE_SIZE = FILE_NOTIFY_CHANGE_SIZE
 , _fILE_NOTIFY_CHANGE_LAST_WRITE = FILE_NOTIFY_CHANGE_LAST_WRITE
 , _fILE_NOTIFY_CHANGE_LAST_ACCESS = FILE_NOTIFY_CHANGE_LAST_ACCESS
 , _fILE_NOTIFY_CHANGE_CREATION = FILE_NOTIFY_CHANGE_CREATION
 , _fILE_NOTIFY_CHANGE_SECURITY = FILE_NOTIFY_CHANGE_SECURITY
 }
