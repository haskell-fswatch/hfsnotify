module Main (main) where

import Control.Monad (void)
import qualified FSNotify.Test
import Test.Sandwich
import TestLib.Util


main :: IO ()
main =
  runSandwichWithCommandLineArgs defaultOptions $
    introduceWith "test folder generator" testFolderGenerator (void . withTestFolderGenerator) $
      parallelN 20 FSNotify.Test.tests
