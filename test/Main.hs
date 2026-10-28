module Main (main) where

import Control.Monad (void)
import qualified FSNotify.Test
import System.IO (BufferMode(..), hSetBuffering, stderr, stdout)
import Test.Sandwich
import TestLib.Util


main :: IO ()
main = do
  -- Debug branch only: keep the diagnostics in order with the formatter's output
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  runSandwichWithCommandLineArgs defaultOptions $
    introduceWith "test folder generator" testFolderGenerator (void . withTestFolderGenerator) $
      parallelN 20 FSNotify.Test.tests
