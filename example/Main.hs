{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Concurrent
import Data.String.Interpolate
import System.FSNotify
import System.FilePath
import UnliftIO.Temporary


main :: IO ()
main = do
  withSystemTempDirectory "fsnotify-foo" $ \dir -> do
    putStrLn [i|Starting watch on dir: #{dir}|]

    let conf = defaultConfig

    withManagerConf conf $ \mgr -> do
      stop <- watchDir mgr dir (const True) $ \ev -> do
        putStrLn [i|Got event: #{ev}|]
      threadDelay 3_000_000

      putStrLn [i|Writing to #{dir </> "bar"}|]
      writeFile (dir </> "bar") "asdf"
      threadDelay 3_000_000

      putStrLn [i|Stopping|]
      stop
      putStrLn [i|Stopped|]
      threadDelay 3_000_000

    putStrLn [i|Exited withManagerConf|]
    threadDelay 3_000_000
