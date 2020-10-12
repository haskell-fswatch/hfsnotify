{-# LANGUAGE CPP, OverloadedStrings, ImplicitParams, MultiWayIf, LambdaCase, RecordWildCards, ViewPatterns #-}

module Main where

import Control.Monad
import EventTests
import Prelude hiding (FilePath)
import System.FSNotify
import Test.Hspec
import Util


main :: IO ()
main = do
  hasNative <- nativeMgrSupported
  unless hasNative $ putStrLn "WARNING: native manager cannot be used or tested on this platform"
  hspec $ do
    describe "SingleThread" $ eventTests SingleThread hasNative
    describe "ThreadPerWatch" $ eventTests ThreadPerWatch hasNative
    describe "ThreadPerEvent" $ eventTests ThreadPerEvent hasNative

    it "respects the confOnHandlerException option" $ pending
