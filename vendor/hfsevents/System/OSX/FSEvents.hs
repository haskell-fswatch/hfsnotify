{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

-- | Event-based file and folder watching for OS X

module System.OSX.FSEvents
  ( EventStream
  , eventStreamCreate
  , eventStreamCreateSince
  , getCurrentEventId
  , eventStreamDestroy
  , Event (..)
  -- event callback flags
  , eventFlagMustScanSubDirs, eventFlagUserDropped, eventFlagKernelDropped
  , eventFlagEventIdsWrapped, eventFlagHistoryDone, eventFlagRootChanged
  , eventFlagMount, eventFlagUnmount
  -- item flags: enable file-level events to get these (OS X 10.7 and higher only)
  , eventFlagItemCreated, eventFlagItemRemoved, eventFlagItemInodeMetaMod
  , eventFlagItemRenamed, eventFlagItemModified, eventFlagItemFinderInfoMod
  , eventFlagItemChangeOwner, eventFlagItemXattrMod
  , eventFlagItemIsFile, eventFlagItemIsDir, eventFlagItemIsSymlink
  -- query api support
  , fileLevelEventsSupported, osVersion
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.Cont
import Data.Bits
import Data.Serialize.Get
import Data.Word
import Data.Int
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO
import System.Posix.IO
import System.Posix.Types

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

data EventStream = EventStream (Ptr CWatch) (MVar Bool)
data Event = Event
   { eventPath  :: FilePath
   , eventId    :: Word64
   , eventFlags :: Word64
   } deriving (Show, Ord, Eq)
data CWatch

eventFlagMustScanSubDirs :: Word64
eventFlagMustScanSubDirs = 0x00000001
eventFlagUserDropped :: Word64
eventFlagUserDropped = 0x00000002
eventFlagKernelDropped :: Word64
eventFlagKernelDropped = 0x00000004
eventFlagEventIdsWrapped :: Word64
eventFlagEventIdsWrapped = 0x00000008
eventFlagHistoryDone :: Word64
eventFlagHistoryDone = 0x00000010
eventFlagRootChanged :: Word64
eventFlagRootChanged = 0x00000020
eventFlagMount :: Word64
eventFlagMount = 0x00000040
eventFlagUnmount :: Word64
eventFlagUnmount = 0x00000080
-- These flags are only set if you enabled file events when creating the stream
eventFlagItemCreated :: Word64
eventFlagItemCreated = 0x00000100
eventFlagItemRemoved :: Word64
eventFlagItemRemoved = 0x00000200
eventFlagItemInodeMetaMod :: Word64
eventFlagItemInodeMetaMod = 0x00000400
eventFlagItemRenamed :: Word64
eventFlagItemRenamed = 0x00000800
eventFlagItemModified :: Word64
eventFlagItemModified = 0x00001000
eventFlagItemFinderInfoMod :: Word64
eventFlagItemFinderInfoMod = 0x00002000
eventFlagItemChangeOwner :: Word64
eventFlagItemChangeOwner = 0x00004000
eventFlagItemXattrMod :: Word64
eventFlagItemXattrMod = 0x00008000
eventFlagItemIsFile :: Word64
eventFlagItemIsFile = 0x00010000
eventFlagItemIsDir :: Word64
eventFlagItemIsDir = 0x00020000
eventFlagItemIsSymlink :: Word64
eventFlagItemIsSymlink = 0x00040000

nest :: [(r -> a) -> a] -> ([r] -> a) -> a
nest xs = runCont (sequence (map cont xs))

withCStringArray0 :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray0 strings act = nest (map withCString strings)
                                     (\rs -> withArray0 nullPtr rs act)

-- | Create an FSEvents watch for a list of paths.
-- The callback action will be called for each event in the watched paths
-- until the 'EventStream' is destroyed again. All callbacks are from a
-- single thread, so if you take too long to process an event, further
-- events will be delayed.
-- Note: it's relatively expensive to create a watch, since each watch
-- uses an operating system thread for its event loop.
eventStreamCreate :: [FilePath]      -- ^ The paths to watch
                  -> Double          -- ^ Latency
                  -> Bool            -- ^ Process event immediately if no other events received for at least latency
                  -> Bool            -- ^ Ignore events caused by current process
                  -> Bool            -- ^ Get file-level notifications instead of directory level 
                  -> (Event -> IO a) -- ^ The action to run when an event has taken place
                  -> IO EventStream  -- ^ The event stream, use this to destroy the stream
eventStreamCreate ps = eventStreamCreateSince ps 0

-- | Like 'eventStreamCreate', but starts from the given event ID rather than
-- \"since now\". Supplying an ID means the stream replays the events after it and
-- then sends a HistoryDone sentinel event, which marks the point from which
-- events are live (and from which the ignore-self flag applies).
eventStreamCreateSince :: [FilePath]      -- ^ The paths to watch
                       -> Word64          -- ^ Event ID to start from, or 0 for \"since now\"
                       -> Double          -- ^ Latency
                       -> Bool            -- ^ Process event immediately if no other events received for at least latency
                       -> Bool            -- ^ Ignore events caused by current process
                       -> Bool            -- ^ Get file-level notifications instead of directory level
                       -> (Event -> IO a) -- ^ The action to run when an event has taken place
                       -> IO EventStream  -- ^ The event stream, use this to destroy the stream
eventStreamCreateSince ps since latency nodefer noself filelevel a = withCStringArray0 ps $ \pp ->
  alloca $ \pfd -> do
  alloca $ \ppw -> do
    when (latency < 0) $ ioError (userError "latency must be nonnegative")
    r <- c_createWatch pp (fromIntegral (length ps)) flags since (realToFrac latency) pfd ppw
    when (r /= 0) $ ioError (userError "could not create file system event stream")
    h <- fdToHandle . Fd =<< peek pfd
    pw <- peek ppw
    destroyed <- newMVar False
    forkIO $ consumeMsgs h a
    return $ EventStream pw destroyed
        where
          flags = condFlag createFlagNoDefer nodefer .|.
                  condFlag createFlagIgnoreSelf noself .|.
                  condFlag createFlagFileEvents filelevel

condFlag :: Word32 -> Bool -> Word32
condFlag f False = 0
condFlag f True  = f

createFlagNoDefer :: Word32
createFlagNoDefer = 0x00000002

createFlagIgnoreSelf :: Word32
createFlagIgnoreSelf = 0x00000008

createFlagFileEvents :: Word32
createFlagFileEvents = 0x00000010

-- |  Destroy an event stream, the callback action will not be run for new events
-- (but there may be pending events remaining)
eventStreamDestroy :: EventStream -> IO ()
eventStreamDestroy (EventStream ptr d) = do
  destroyed <- takeMVar d
  when (not destroyed) $ c_destroyWatch ptr
  putMVar d True

consumeMsgs :: Handle -> (Event -> IO a) -> IO ()
consumeMsgs h a = readEvents
    where
      readEvents = do
        b <- B.hGet h 24
        if B.length b < 24
          then stop
          else do
            let header = runGet readHeader b
            case header of
              Left _ -> stop
              Right (eventId, flags, pathLen) -> do 
                bp <- B.hGet h (fromIntegral pathLen)
                if B.length bp /= fromIntegral pathLen
                  then stop
                  else do
                    let p = TE.decodeUtf8With TE.lenientDecode bp
                    a $ Event (T.unpack p) eventId flags
                    readEvents

      stop = hClose h >> return ()
      readHeader = liftM3 (,,) getWord64host getWord64host getWord64host

osVersion :: IO (Integer, Integer, Integer)
osVersion = alloca $ \major -> do
            alloca $ \minor -> do
            alloca $ \bugfix -> do
              c_osVersion major minor bugfix
              liftM3 (,,) (peekfi major) (peekfi minor) (peekfi bugfix)
  where
    peekfi = fmap fromIntegral . peek

fileLevelEventsSupported :: IO Bool
fileLevelEventsSupported = fmap (>= (10,7,0)) osVersion

foreign import ccall safe "c_fsevents.h createWatch" c_createWatch :: Ptr (Ptr CChar)
                                                                   -> CInt
                                                                   -> Word32
                                                                   -> Word64
                                                                   -> CDouble
                                                                   -> Ptr (CInt)
                                                                   -> Ptr (Ptr CWatch)
                                                                   -> IO CInt

-- | The most recently generated event ID, system-wide. Pass this to
-- 'eventStreamCreateSince' to start a stream without a gap.
getCurrentEventId :: IO Word64
getCurrentEventId = c_getCurrentEventId

foreign import ccall safe "c_fsevents.h getCurrentEventId" c_getCurrentEventId :: IO Word64

foreign import ccall safe "c_fsevents.h destroyWatch" c_destroyWatch :: Ptr CWatch
                                                                     -> IO ()
foreign import ccall safe "c_fsevents.h osVersion" c_osVersion :: Ptr Int32
                                                               -> Ptr Int32
                                                               -> Ptr Int32
                                                               -> IO ()

