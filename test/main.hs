module Main where

import Prelude hiding (FilePath)

import Control.Concurrent
import Filesystem.Path.CurrentOS
import System.IO.FSNotify
import System.IO.FSNotify.Types

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
    pollId  <- watchDirAction pollMan (fp ".") act (\event -> do
                                                       print event
                                                       print "Blocking on path"
                                                       threadDelay 5000000
                                                       print "Unblocking on path")
    print pollId
    putStrLn "Listens to '.'; Hit enter to terminate."
    _       <- getLine
    stopManager pollMan
