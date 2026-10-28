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

      withWatchedTempDirectory ReusedDir conf $ \watchedDir' readEvents -> do
        let ourFile = watchedDir' </> "testfile"
        liftIO $ writeFile ourFile "foo"

        if | isMac && ignoreSelf -> do
               threadDelay 2_000_000

               events <- liftIO readEvents
               unless (null events) $
                 expectationFailure $ describeUnexpected "control" watchedDir' events
           | otherwise -> waitUntil 5.0 $ do
               events <- liftIO readEvents
               unless (any ((`equalFilePath` ourFile) . eventPath) events) $
                 expectationFailure [i|Expected an event for #{ourFile}, but got: #{events}|]

  when isMac $ do
    -- How wide is the window where the flag doesn't apply? Each of these waits a different
    -- amount after the watch starts before writing, so the failure counts profile the window.
    let settles = [("immediate", 0), ("50ms", 50_000), ("200ms", 200_000), ("1s", 1_000_000)] :: [(String, Int)]
    forM_ settles $
      \(settleLabel, settleMicros) ->
        it [i|filters our own write (settle #{settleLabel})|] $ do
          let conf = defaultConfig { confIgnoreSelf = True }

          withWatchedTempDirectory UniqueDir conf $ \watchedDir' readEvents -> do
            when (settleMicros > 0) $ threadDelay settleMicros
            liftIO $ writeFile (watchedDir' </> "testfile") "foo"
            threadDelay 2_000_000

            events <- liftIO readEvents
            unless (null events) $
              expectationFailure $ describeUnexpected [i|settle #{settleLabel}|] watchedDir' events

    -- Instead of waiting a fixed time, wait for proof that the stream is delivering: another
    -- process touches a sentinel file, and the flag never filters another process's events. If
    -- this never leaks, it's a readiness signal we can wait on rather than a sleep to tune.
    it "filters our own write (sentinel handshake)" $ do
      let conf = defaultConfig { confIgnoreSelf = True }

      withWatchedTempDirectory UniqueDir conf $ \watchedDir' readEvents -> do
        let sentinel = watchedDir' </> "sentinel"
        callProcess "touch" [sentinel]

        waitUntil 10.0 $ do
          events <- liftIO readEvents
          unless (any ((`equalFilePath` sentinel) . eventPath) events) $
            expectationFailure [i|Sentinel event for #{sentinel} hasn't arrived yet|]

        liftIO $ writeFile (watchedDir' </> "testfile") "foo"
        threadDelay 2_000_000

        events <- liftIO readEvents
        let oursOnly = filter (not . (`equalFilePath` sentinel) . eventPath) events
        unless (null oursOnly) $
          expectationFailure $ describeUnexpected "sentinel handshake" watchedDir' oursOnly

    it "still reports other processes' events when confIgnoreSelf is set" $ do
      let conf = defaultConfig { confIgnoreSelf = True }

      withWatchedTempDirectory ReusedDir conf $ \watchedDir' readEvents -> do
        let theirFile = watchedDir' </> "testfile"
        callProcess "touch" [theirFile]

        waitUntil 5.0 $ do
          events <- liftIO readEvents
          unless (any ((`equalFilePath` theirFile) . eventPath) events) $
            expectationFailure [i|Expected an event for #{theirFile}, but got: #{events}|]

-- | Say, for each unexpected event, whether it happened in the directory this watch was for.
-- A foreign directory means the event reached the wrong watch; our own means FSEvents reported
-- a change we made despite being asked not to.
describeUnexpected :: String -> FilePath -> [Event] -> String
describeUnexpected label watchedDir' events = [i|[#{label}] Expected no events while watching #{watchedDir'}, but got #{length events}:
#{unlines (fmap describeEvent events)}|]
  where
    describeEvent ev
      | takeDirectory (eventPath ev) `equalFilePath` watchedDir' = [i|  OWN DIRECTORY: #{ev}|]
      | otherwise = [i|  FOREIGN DIRECTORY: #{ev}|]
