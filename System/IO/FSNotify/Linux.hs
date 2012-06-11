--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE MultiParamTypeClasses #-}

module System.IO.FSNotify.Linux
  (
  ) where

import Prelude hiding (FilePath)

import Filesystem.Path.CurrentOS
import System.FilePath.Find
import System.IO hiding (FilePath)
import System.IO.FSNotify
import qualified System.INotify as INo

-- Helper functions for dealing with String vs. FileSystem.Path.CurrentOS.FilePath
fp :: String -> FilePath
fp = decodeString
str :: FilePath -> String
str = encodeString

fsnEvent :: INo.Event -> Maybe Event
fsnEvent (INo.Created False name)          = Just (Added (fp name))
fsnEvent (INo.MovedIn False name cookie)   = Just (Added (fp name))
fsnEvent (INo.Modified False (Just name))  = Just (Modified (fp name))
fsnEvent (INo.Deleted  False name)         = Just (Removed (fp name))
fsnEvent (INo.MovedOut  False name cookie) = Just (Removed (fp name))
fsnEvent _                                 = Nothing

handleInoEvent :: ActionPredicate -> Action -> INo.Event -> IO ()
handleInoEvent actPred action inoEvent = handleEvent actPred action (fsnEvent inoEvent)
handleEvent :: ActionPredicate -> Action -> Maybe Event -> IO ()
handleEvent actPred action (Just event) = if actPred event then action event else return ()
handlEvent _ _ Nothing = return ()

instance ListenerSession INo.INotify where
  initSession = INo.initINotify
  killSession = INo.killINotify

instance FileListener INo.INotify INo.WatchDescriptor where
  listen iNotify path actPred action =
    INo.addWatch iNotify varieties (str path) handler
    where
      varieties = [INo.MoveIn, INo.MoveOut, INo.CloseWrite]
      handler :: INo.Event -> IO ()
      handler = handleInoEvent actPred action

  rlisten iNotify path actPred action = do
    paths <- find always (fileType ==? Directory) (str path)
    mapM_ (\filePath -> INo.addWatch iNotify newDirVarieties filePath newDirHandler) paths
    mapM (\filePath -> INo.addWatch iNotify actionVarieties filePath handler) paths
    where
      newDirVarieties = [INo.Create]
      actionVarieties = [INo.MoveIn, INo.MoveOut, INo.CloseWrite]
      newDirHandler :: INo.Event -> IO ()
      newDirHandler (INo.Created _ name) = do
        (rlisten iNotify  (path </> (fp name)) actPred action) :: IO [INo.WatchDescriptor]
        return ()
      handler :: INo.Event -> IO ()
      handler = handleInoEvent actPred action
