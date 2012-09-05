import System.IO.FSNotify
import Filesystem
-- {-# LANGUAGE OverloadedStrings #-}
-- import Filesystem.Path.CurrentOS

main :: IO ()
main = do
  -- let wd = "."
  wd <- getWorkingDirectory
  print wd
  withManager $ \man ->
    watchTree man wd (const True) print
  _ <- getLine
  return ()
