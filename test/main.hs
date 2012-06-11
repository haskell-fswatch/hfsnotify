{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Prelude hiding (FilePath)

import Control.Concurrent
import Filesystem.Path.CurrentOS
import System.IO.FSNotify.Types

fp :: String -> FilePath
fp = decodeString
str :: FilePath -> String
str = encodeString
mapFP :: [String] -> [FilePath]
mapFP = map fp
mapStr :: [FilePath] -> [String]
mapStr = map str

-- TODO: This uses () to use polling

main :: IO ()
main = do
    pollMan <- initSession :: IO PollManager
    pollId <- listen pollMan (fp ".") (\event -> True) print
    print pollId
    putStrLn "Listens to '.'; Hit enter to terminate."
    getLine
    killSession pollMan
