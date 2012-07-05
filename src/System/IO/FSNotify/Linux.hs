--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.IO.FSNotify.Linux
       ( FileListener(..)
       , NativeManager
       ) where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan
import Control.Exception
import Data.Typeable
import Filesystem.Path.CurrentOS
import System.IO.FSNotify.Listener
import System.IO.FSNotify.Path
import System.IO.FSNotify.Types
import qualified System.INotify as INo

type NativeManager = INo.INotify

data EventVarietyMismatchException = EventVarietyMismatchException deriving (Show, Typeable)
instance Exception EventVarietyMismatchException

-- Note that INo.Closed in this context is "modified" because we listen to
-- CloseWrite events.
fsnEvent :: INo.Event -> Maybe Event
fsnEvent (INo.Closed   False (Just name) _) = Just (Modified (fp name))
fsnEvent (INo.MovedOut False       name  _) = Just (Removed  (fp name))
fsnEvent (INo.MovedIn  False       name  _) = Just (Added    (fp name))
fsnEvent (INo.Deleted  False       name   ) = Just (Removed  (fp name))
fsnEvent _                                  = Nothing

handleInoEvent ::  ActionPredicate -> EventChannel -> INo.Event -> IO ()
handleInoEvent actPred chan inoEvent = handleEvent actPred chan (fsnEvent inoEvent)

handleEvent :: ActionPredicate -> EventChannel -> Maybe Event -> IO ()
handleEvent actPred chan (Just event)
  | actPred event       = writeChan chan event
  | otherwise           = return ()
handleEvent _ _ Nothing = return ()

instance FileListener INo.INotify where
  initSession = fmap Just INo.initINotify

  killSession = INo.killINotify

  listen iNotify path actPred chan = do
    _ <- INo.addWatch iNotify varieties (encodeString path) handler
    return ()
    where
      varieties = [INo.Create, INo.Delete, INo.MoveIn, INo.MoveOut, INo.CloseWrite]
      handler :: INo.Event -> IO ()
      handler = handleInoEvent actPred chan

  rlisten iNotify path actPred chan = do
    paths <- findDirs True path
    mapM_ (\filePath -> INo.addWatch iNotify newDirVarieties (fp filePath) newDirHandler) paths
    mapM_ (\filePath -> INo.addWatch iNotify actionVarieties (fp filePath) handler) paths
    where
      newDirVarieties = [INo.Create]
      actionVarieties = [INo.MoveIn, INo.MoveOut, INo.CloseWrite]

      newDirHandler :: INo.Event -> IO ()
      newDirHandler (INo.Created _ name) =
        rlisten iNotify (path </> fp name) actPred chan
      newDirHandler _ = throw EventVarietyMismatchException

      handler :: INo.Event -> IO ()
      handler = handleInoEvent actPred chan
