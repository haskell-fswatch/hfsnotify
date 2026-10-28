{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module FSNotify.Test.IgnoreSelf (
  tests
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.String.Interpolate
import System.FSNotify
import System.FilePath
import Test.Sandwich
import TestLib.Util
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Process (callProcess)


tests :: TestSpec
tests = describe "Ignore self" $ parallelWithoutDirectory $ do
  -- confIgnoreSelf is implemented with the FSEvents kFSEventStreamCreateFlagIgnoreSelf flag,
  -- so it only filters our own events on macOS. Everywhere else it does nothing.
  forM_ [False, True] $ \ignoreSelf ->
    it [i|respects the confIgnoreSelf option (#{ignoreSelf})|] $ do
      let conf = defaultConfig { confIgnoreSelf = ignoreSelf }

      withWatchedTempDirectory conf $ \watchedDir' readEvents -> do
        let ourFile = watchedDir' </> "testfile"
        liftIO $ writeFile ourFile "foo"

        if | isMac && ignoreSelf -> do
               threadDelay 2_000_000

               events <- liftIO readEvents
               unless (null events) $
                 expectationFailure $ describeUnexpected watchedDir' events
           | otherwise -> waitUntil 5.0 $ do
               events <- liftIO readEvents
               unless (any ((`equalFilePath` ourFile) . eventPath) events) $
                 expectationFailure [i|Expected an event for #{ourFile}, but got: #{events}|]

  when isMac $
    it "still reports other processes' events when confIgnoreSelf is set" $ do
      let conf = defaultConfig { confIgnoreSelf = True }

      withWatchedTempDirectory conf $ \watchedDir' readEvents -> do
        let theirFile = watchedDir' </> "testfile"
        callProcess "touch" [theirFile]

        waitUntil 5.0 $ do
          events <- liftIO readEvents
          unless (any ((`equalFilePath` theirFile) . eventPath) events) $
            expectationFailure [i|Expected an event for #{theirFile}, but got: #{events}|]

-- | Say, for each unexpected event, whether it happened in the directory this watch was for.
-- A foreign directory means the event reached the wrong watch; our own means FSEvents reported
-- a change we made despite being asked not to.
describeUnexpected :: FilePath -> [Event] -> String
describeUnexpected watchedDir' events = [i|Expected no events while watching #{watchedDir'}, but got #{length events}:
#{unlines (fmap describeEvent events)}|]
  where
    describeEvent ev
      | takeDirectory (eventPath ev) `equalFilePath` watchedDir' = [i|  OWN DIRECTORY: #{ev}|]
      | otherwise = [i|  FOREIGN DIRECTORY: #{ev}|]
