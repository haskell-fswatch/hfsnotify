-- Temporary experiment harness (not for merge): measures how often an event fails to arrive
-- inside the window the test suite allows, and whether it shows up late afterwards or never at
-- all. "Late" means delivery latency; "Lost" means the event really is gone.

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


-- | The window the test suite allows (waitUntil 5.0).
suiteWindowSecs :: Double
suiteWindowSecs = 5

-- | How much longer we keep waiting after that, to see whether the event was merely late.
graceSecs :: Double
graceSecs = 25

data Outcome =
  InTime Double
  | Late Double
  | Lost

-- | Watch a fresh directory and create a file in it immediately, with no pause for the watch to
-- settle. This is what every test in the suite does.
freshWatchTrial :: IO Outcome
freshWatchTrial = withSystemTempDirectory "fsnotify-stress" $ \dir -> do
  arrivedAt <- newIORef Nothing

  withManager $ \mgr -> do
    stop <- watchDir mgr dir (const True) $ \_ev -> recordArrival arrivedAt

    startedAt <- getMonotonicTime
    writeFile (dir </> "testfile") "foo"
    outcome <- awaitOutcome arrivedAt startedAt

    stop
    return outcome

-- | Many writes through a single established watch, which is where most of the suite's
-- assertions actually sit.
steadyStateTrials :: Int -> Int -> IO [Outcome]
steadyStateTrials watchers writesPerWatcher = fmap concat $ forM [1 .. watchers] $ \_ ->
  withSystemTempDirectory "fsnotify-stress" $ \dir -> do
    wanted <- newIORef ""
    arrivedAt <- newIORef Nothing

    withManager $ \mgr -> do
      stop <- watchDir mgr dir (const True) $ \ev -> do
        name <- readIORef wanted
        when (takeFileName (eventPath ev) == name) $ recordArrival arrivedAt

      outcomes <- forM [1 .. writesPerWatcher] $ \i -> do
        let name = "file" <> show i
        writeIORef arrivedAt Nothing
        writeIORef wanted name

        startedAt <- getMonotonicTime
        writeFile (dir </> name) "foo"
        awaitOutcome arrivedAt startedAt

      stop
      return outcomes

recordArrival :: IORef (Maybe Double) -> IO ()
recordArrival arrivedAt = do
  now <- getMonotonicTime
  atomicModifyIORef' arrivedAt $ \previous -> (maybe (Just now) Just previous, ())

awaitOutcome :: IORef (Maybe Double) -> Double -> IO Outcome
awaitOutcome arrivedAt startedAt = go
  where
    go = readIORef arrivedAt >>= \case
      Just at -> do
        let elapsed = at - startedAt
        return $ if elapsed <= suiteWindowSecs then InTime (elapsed * 1000) else Late (elapsed * 1000)
      Nothing -> do
        now <- getMonotonicTime
        if now - startedAt > suiteWindowSecs + graceSecs
          then return Lost
          else threadDelay 1_000 >> go

report :: String -> [Outcome] -> IO ()
report name outcomes = do
  putStrLn $ "PHASE " <> name
  putStrLn $ "  trials: " <> show (length outcomes)
             <> "  in_time: " <> show (length inTime)
             <> "  late: " <> show (length late)
             <> "  lost: " <> show (length [() | Lost <- outcomes])
  unless (null inTime) $
    putStrLn $ "  in_time_ms: p50=" <> percentile 0.5 <> " p90=" <> percentile 0.9
                                    <> " p99=" <> percentile 0.99 <> " max=" <> twoDecimals (last sorted)
  unless (null late) $
    putStrLn $ "  late_ms: " <> show (map (twoDecimals) late)
  where
    inTime = [ms | InTime ms <- outcomes]
    late = [ms | Late ms <- outcomes]
    sorted = sort inTime
    percentile p = twoDecimals $ sorted !! min (length sorted - 1) (floor (p * fromIntegral (length sorted)))

twoDecimals :: Double -> String
twoDecimals x = show (fromIntegral (round (x * 100) :: Int) / 100 :: Double)

main :: IO ()
main = do
  report "fresh-watch" =<< replicateM 300 freshWatchTrial

  -- 20 at a time, the concurrency the suite runs at
  report "fresh-watch-parallel20" . concat =<< replicateM 25 (mapConcurrently (const freshWatchTrial) [1 .. 20 :: Int])

  report "steady-state" =<< steadyStateTrials 25 400
