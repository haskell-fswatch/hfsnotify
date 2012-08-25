module Main where

import Test.Hspec (hspec, Spec)
import qualified Path as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = P.spec
