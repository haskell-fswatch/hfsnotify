--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.Linux
  (
  ) where

import System.IO
import Text.Regex.TDFA
import System.FilePath.Find as Find

import System.IO.FSNotify
import qualified System.INotify as INo

fsnEvent :: INo.Event -> Event
fsnEvent (INo.Created  _ name) = Created  name
fsnEvent (INo.Modified _ name) = Modified name
fsnEvent (INo.Deleted  _ name) = Deleted  name

actWhenMatched :: FilePath -> FilePattern -> Action -> INo.Event -> IO ()
actWhenMatched path pattern action event
  | (path =~ pattern) = action (fsnEvent event)
  | otherwise         = return ()

instance ListenerSession INo.INotify where
  initSession = INo.initINotify
  killSession = INo.killINotify

instance FileListener INo.INotify INo.WatchDescriptor where
  listen iNotify path pattern action =
    INo.addWatch iNotify varieties path realAction
    where
    varieties = [INo.Created, INo.Modified, INo.Deleted]
    realAction = actWhenMatched path pattern action

  rlisten iNotify path pattern action = do
    paths <- Find.find always (fileType ==? Directory) path
    mapM (\fd -> INo.addWatch iNotify newDirVarieties (Find.filePath fd) newDirAction) paths
    mapM (\fd -> INo.addWatch iNotify actionVarieties (Find.filePath fd) realAction) paths
    where
    newDirVarieties = [INo.Created]
    actionVarieties = [INo.Created, INo.Modified, INo.Deleted]
    newDirAction (INo.Created _ name) = rlisten iNotify (path ++ "/" ++ name) pattern action
    realAction = actWhenMatched path pattern action
