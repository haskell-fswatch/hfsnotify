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

  when isMac $ do
    -- FSEvents filters on a best-effort basis: we've seen it report a write of ours anyway, so
    -- asserting that every attempt is filtered is flaky. Asking for one filtered attempt out of
    -- several still fails if the flag isn't being passed at all, since then every attempt reports.
    it "filters our own events when confIgnoreSelf is set" $ do
      let conf = defaultConfig { confIgnoreSelf = True }

      attempts <- forM [1 .. 3 :: Int] $ \_ ->
        withWatchedTempDirectory conf $ \watchedDir' readEvents -> do
          liftIO $ writeFile (watchedDir' </> "testfile") "foo"
          threadDelay 2_000_000
          liftIO readEvents

      unless (any null attempts) $
        expectationFailure [i|Expected at least one attempt with no events, but got: #{attempts}|]

    it "still reports other processes' events when confIgnoreSelf is set" $ do
      let conf = defaultConfig { confIgnoreSelf = True }

      withWatchedTempDirectory conf $ \watchedDir' readEvents -> do
        let theirFile = watchedDir' </> "testfile"
        callProcess "touch" [theirFile]

        waitUntil 5.0 $ do
          events <- liftIO readEvents
          unless (any ((`equalFilePath` theirFile) . eventPath) events) $
            expectationFailure [i|Expected an event for #{theirFile}, but got: #{events}|]
