{-# LANGUAGE FlexibleContexts #-}
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
  -- confIgnoreSelf is implemented with the FSEvents kFSEventStreamCreateFlagIgnoreSelf flag, so
  -- it only filters anything on macOS. Everywhere else it does nothing, which is what we check
  -- here by expecting our own write to be reported with the option both off and on.
  forM_ [False, True] $ \ignoreSelf -> unless (isMac && ignoreSelf) $
    it [i|reports our own events with confIgnoreSelf = #{ignoreSelf}|] $ do
      let conf = defaultConfig { confIgnoreSelf = ignoreSelf }

      withWatchedTempDirectory conf $ \watchedDir' readEvents -> do
        let ourFile = watchedDir' </> "testfile"
        liftIO $ writeFile ourFile "foo"

        waitUntil 5.0 $ do
          events <- liftIO readEvents
          unless (any ((`equalFilePath` ourFile) . eventPath) events) $
            expectationFailure [i|Expected an event for #{ourFile}, but got: #{events}|]

  when isMac $
    it "filters our own events when confIgnoreSelf is set" $ do
      let conf = defaultConfig { confIgnoreSelf = True }

      withWatchedTempDirectory conf $ \watchedDir' readEvents -> do
        -- FSEvents can report a change we made in the moments right after a watch starts, before
        -- it has us on file as the process responsible. So first wait for an event it has no
        -- reason to filter, one from another process, which tells us the watch is really going.
        let sentinel = watchedDir' </> "sentinel"
        callProcess "touch" [sentinel]

        waitUntil 10.0 $ do
          events <- liftIO readEvents
          unless (any ((`equalFilePath` sentinel) . eventPath) events) $
            expectationFailure [i|Expected an event for #{sentinel}, but got: #{events}|]

        liftIO $ writeFile (watchedDir' </> "testfile") "foo"
        threadDelay 2_000_000

        events <- liftIO readEvents
        let ourEvents = filter (not . (`equalFilePath` sentinel) . eventPath) events
        unless (null ourEvents) $
          expectationFailure [i|Expected no events for our own write in #{watchedDir'}, but got: #{ourEvents}|]
