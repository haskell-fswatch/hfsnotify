-- Temporary experiment harness (not for merge): measures how often an event is missed and how
-- long events take to arrive, to tell event loss apart from delivery latency.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.List (sort)
import GHC.Clock (getMonotonicTime)
import System.FSNotify
import System.FilePath
import UnliftIO.Async (mapConcurrently)
import UnliftIO.Temporary (withSystemTempDirectory)


trials :: Int
trials = 100

-- | How long to keep waiting for an event before calling it missed. The test suite allows 5s, so
-- anything beyond this is a miss by any reasonable standard.
missedAfterSecs :: Double
missedAfterSecs = 10

-- | One trial: watch a fresh directory, wait @settleMicros@, create a file in it, and report how
-- many milliseconds the first event took to arrive ('Nothing' if it never did).
trial :: Int -> IO (Maybe Double)
trial settleMicros = withSystemTempDirectory "fsnotify-stress" $ \dir -> do
  arrivedAt <- newIORef Nothing

  withManager $ \mgr -> do
    stop <- watchDir mgr dir (const True) $ \_ev -> do
      now <- getMonotonicTime
      atomicModifyIORef' arrivedAt $ \previous -> (maybe (Just now) Just previous, ())

    when (settleMicros > 0) $ threadDelay settleMicros

    startedAt <- getMonotonicTime
    writeFile (dir </> "testfile") "foo"
    result <- waitForEvent arrivedAt startedAt

    stop
    return result

waitForEvent :: IORef (Maybe Double) -> Double -> IO (Maybe Double)
waitForEvent arrivedAt startedAt = go
  where
    go = readIORef arrivedAt >>= \case
      Just at -> return $ Just ((at - startedAt) * 1000)
      Nothing -> do
        now <- getMonotonicTime
        if now - startedAt > missedAfterSecs
          then return Nothing
          else threadDelay 1_000 >> go

report :: String -> [Maybe Double] -> IO ()
report name results = do
  putStrLn $ "PHASE " <> name
  putStrLn $ "  trials: " <> show (length results) <> "  missed: " <> show (length [() | Nothing <- results])
  unless (null arrived) $
    putStrLn $ "  latency_ms: p50=" <> percentile 0.5 <> " p90=" <> percentile 0.9
                                    <> " p99=" <> percentile 0.99 <> " max=" <> twoDecimals (last arrived)
  where
    arrived = sort [at | Just at <- results]
    percentile p = twoDecimals $ arrived !! min (length arrived - 1) (floor (p * fromIntegral (length arrived)))

twoDecimals :: Double -> String
twoDecimals x = show (fromIntegral (round (x * 100) :: Int) / 100 :: Double)

main :: IO ()
main = do
  -- Write with no pause after the watch starts: this is what every test in the suite does, and
  -- what the startup race would break
  report "immediate" =<< replicateM trials (trial 0)

  -- Same, but give the watch half a second first. If this one is clean while "immediate" isn't,
  -- the problem is the startup window rather than general latency
  report "settled-500ms" =<< replicateM trials (trial 500_000)

  -- 20 watches at once, which is the kind of load the test suite runs under (parallelN 20)
  report "immediate-parallel20" . concat =<< replicateM 5 (mapConcurrently (const (trial 0)) [1 .. 20 :: Int])
