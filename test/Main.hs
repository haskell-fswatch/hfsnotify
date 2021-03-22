{-# LANGUAGE CPP, OverloadedStrings, ImplicitParams, MultiWayIf, LambdaCase, RecordWildCards, ViewPatterns #-}

module Main where

import Control.Exception.Safe
import Control.Monad
import Data.IORef
import FSNotify.Test.EventTests
import FSNotify.Test.Util
import Prelude hiding (FilePath)
import System.FSNotify
import System.FilePath
import Test.Hspec


main :: IO ()
main = do
  hspec $ do
    describe "Configuration" $ do
      it "respects the confOnHandlerException option" $ do
        withRandomTempDirectory $ \watchedDir -> do
          exceptions <- newIORef (0 :: Int)
          let conf = defaultConfig { confOnHandlerException = \_ -> modifyIORef exceptions (+ 1) }

          withManagerConf conf $ \mgr -> do
            stop <- watchDir mgr watchedDir (const True) $ \ev -> do
              case ev of
                Added {} -> throwIO $ userError "Oh no!"
                _ -> return ()

            writeFile (watchedDir </> "testfile") "foo"

            let ?timeInterval = 5*10^(5 :: Int)
            pauseAndRetryOnExpectationFailure 3 $
              readIORef exceptions >>= (`shouldBe` 1)

            stop

    describe "SingleThread" $ eventTests SingleThread
    describe "ThreadPerWatch" $ eventTests ThreadPerWatch
    describe "ThreadPerEvent" $ eventTests ThreadPerEvent
