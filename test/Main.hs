{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
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
import UnliftIO.IORef


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions $ parallelN 20 $ do
  describe "Configuration" $ do
    it "respects the confOnHandlerException option" $ do
      withRandomTempDirectory $ \watchedDir' -> do
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

  describe "SingleThread" $ eventTests SingleThread
  describe "ThreadPerWatch" $ eventTests ThreadPerWatch
  describe "ThreadPerEvent" $ eventTests ThreadPerEvent
