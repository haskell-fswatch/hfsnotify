{-# LANGUAGE CPP, OverloadedStrings, ImplicitParams, MultiWayIf, LambdaCase, RecordWildCards, ViewPatterns #-}

module Main where

import Control.Exception.Safe
import Control.Monad
import FSNotify.Test.EventTests
import FSNotify.Test.Util
import Prelude hiding (FilePath)
import System.FSNotify
import Test.Hspec


main :: IO ()
main = do
  hasNative <- nativeMgrSupported
  unless hasNative $ throwIO $ userError "WARNING: native manager cannot be used or tested on this platform"

  hspec $ do
    it "respects the confOnHandlerException option" $ pending

    describe "SingleThread" $ eventTests SingleThread
    describe "ThreadPerWatch" $ eventTests ThreadPerWatch
    describe "ThreadPerEvent" $ eventTests ThreadPerEvent
