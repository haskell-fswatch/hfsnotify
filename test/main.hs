{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Prelude hiding (FilePath)

import Control.Concurrent
import Filesystem.Path.CurrentOS
import System.IO.FSNotify

fp :: String -> FilePath
fp = decodeString
str :: FilePath -> String
str = encodeString
mapFP :: [String] -> [FilePath]
mapFP = map fp
mapStr :: [FilePath] -> [String]
mapStr = map str

main :: IO ()
main = do
    pollMan <- startManager
    pollId  <- watch pollMan (fp ".") (\_ -> True) print
    print pollId
    putStrLn "Listens to '.'; Hit enter to terminate."
    getLine
    stopManager pollMan
