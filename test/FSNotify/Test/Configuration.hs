{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module FSNotify.Test.Configuration (
  tests
  ) where

import Control.Monad.IO.Class
import Data.String.Interpolate
import System.FSNotify
import System.FilePath
import Test.Sandwich
import TestLib.Util
import UnliftIO.IORef


tests :: TestSpec
tests = describe "Configuration" $
  it "respects the confOnHandlerException option" $
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
