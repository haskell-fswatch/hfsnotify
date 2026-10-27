{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.String.Interpolate
import FSNotify.Test.EventTests
import FSNotify.Test.Util
import Prelude hiding (FilePath)
import System.FSNotify
import System.FilePath
import Test.Sandwich
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.IORef
import UnliftIO.Process (callProcess)


main :: IO ()
main =
  withTestFolderGenerator $ \testFolderGenerator ->
  runSandwichWithCommandLineArgs defaultOptions $ parallelN 20 $ do
    describe "Configuration" $ do
      it "respects the confOnHandlerException option" $ do
        withRandomTempDirectory testFolderGenerator $ \watchedDir' -> do
          info [i|Got temp dir: #{watchedDir'}|]
          exceptions <- newIORef (0 :: Int)
          let conf = defaultConfig { confOnHandlerException = \_ -> modifyIORef exceptions (+ 1) }

          liftIO $ withManagerConf conf $ \mgr -> do
            stop <- watchDir mgr watchedDir' (const True) $ \ev -> do
              case ev of
#ifdef darwin_HOST_OS
                Modified {} -> expectationFailure "Oh no!"
#else
                Added {} -> expectationFailure "Oh no!"
#endif
                _ -> return ()

            writeFile (watchedDir' </> "testfile") "foo"

            waitUntil 5.0 $
              readIORef exceptions >>= (`shouldBe` 1)

            stop

      -- confIgnoreSelf is implemented with the FSEvents kFSEventStreamCreateFlagIgnoreSelf flag,
      -- so it only filters our own events on macOS. Everywhere else it does nothing.
      forM_ [False, True] $ \ignoreSelf ->
        it [i|respects the confIgnoreSelf option (#{ignoreSelf})|] $ do
          let conf = defaultConfig { confIgnoreSelf = ignoreSelf }

          withWatchedTempDirectory testFolderGenerator conf $ \watchedDir' getEvents' -> do
            let ourFile = watchedDir' </> "testfile"
            liftIO $ writeFile ourFile "foo"

            if | isMac && ignoreSelf -> do
                   threadDelay 2_000_000
                   liftIO getEvents' >>= (`shouldBe` [])
               | otherwise -> waitUntil 5.0 $ do
                   events <- liftIO getEvents'
                   unless (any ((`equalFilePath` ourFile) . eventPath) events) $
                     expectationFailure [i|Expected an event for #{ourFile}, but got: #{events}|]

      when isMac $
        it "still reports other processes' events when confIgnoreSelf is set" $ do
          let conf = defaultConfig { confIgnoreSelf = True }

          withWatchedTempDirectory testFolderGenerator conf $ \watchedDir' getEvents' -> do
            let theirFile = watchedDir' </> "testfile"
            callProcess "touch" [theirFile]

            waitUntil 5.0 $ do
              events <- liftIO getEvents'
              unless (any ((`equalFilePath` theirFile) . eventPath) events) $
                expectationFailure [i|Expected an event for #{theirFile}, but got: #{events}|]

    describe "SingleThread" $ eventTests testFolderGenerator SingleThread
    describe "ThreadPerWatch" $ eventTests testFolderGenerator ThreadPerWatch
    describe "ThreadPerEvent" $ eventTests testFolderGenerator ThreadPerEvent
