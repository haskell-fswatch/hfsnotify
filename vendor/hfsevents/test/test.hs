module Main where

import System.OSX.FSEvents

import Control.Monad
import Control.Concurrent

main = do
  es <- eventStreamCreate ["/Users"] 1.0 True True True print
  replicateM 30 (threadDelay 1000000)
  putStrLn "destroying event stream"
  eventStreamDestroy es
  replicateM 30 (threadDelay 1000000)

