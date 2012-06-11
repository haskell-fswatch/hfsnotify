{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Prelude hiding (FilePath)

import Control.Concurrent
import Filesystem.Path.CurrentOS
import System.Directory
import System.IO hiding (FilePath)
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
    initSession :: IO ()
    hid <- (listen () (fp "/tmp/hfsnotify") (\event -> True) print :: IO ThreadId)
    print hid
    putStrLn "Listens to /tmp/inotify. Hit enter to terminate."
    getLine
    killThread hid
    killSession ()
