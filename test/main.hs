module Main where

import Test.Hspec (hspec, Spec)
import qualified Path as P
import qualified FSNotify as FSN

main :: IO ()
main = hspec spec

spec :: Spec
spec = P.spec >> FSN.spec
