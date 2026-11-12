{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module FSNotify.Test.WatchedDirectory (
  tests
  ) where

import System.FSNotify
import Test.Sandwich
import TestLib.Util
import UnliftIO.Directory


tests :: TestSpec
tests = describe "Watched directory" $ parallelWithoutDirectory $ do
  it "can be renamed while watched" $ withWatchedTempDirectory defaultConfig $ \dir _getEvents -> do
    renameDirectory dir (dir <> "-renamed")
    renameDirectory (dir <> "-renamed") dir

  it "can be removed while watched" $ withWatchedTempDirectory defaultConfig $ \dir _getEvents ->
    removeDirectory dir
