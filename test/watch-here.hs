import System.IO.FSNotify
import Filesystem
-- {-# LANGUAGE OverloadedStrings #-}
-- import Filesystem.Path.CurrentOS

main :: IO ()
main = do
  -- let wd = "."
  wd <- getWorkingDirectory
  print wd
  withManager DebounceDefault $ \man -> do
    watchTree man wd (const True) print
    getLine
  return ()
