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
import System.Directory (createDirectory)
import System.FSNotify
import System.FilePath
import UnliftIO.Async (mapConcurrently)
import UnliftIO.Temporary (withSystemTempDirectory)


-- | Events normally arrive in well under 20ms, so anything slower than this is as good as missed
-- (the suite allows 5s).
suiteWindowSecs :: Double
suiteWindowSecs = 2

-- | How much longer we keep waiting after that, to see whether the event was merely late.
graceSecs :: Double
graceSecs = 3

data Outcome =
  InTime Double
  | Late Double
  | Lost

-- | Watch a fresh directory and act on it after @settleMicros@, with no pause by default. This is
-- what every test in the suite does.
--
-- Which watch delivers the event matters on Windows: watchDir there opens one watch for file
-- flags and then a second for directory-name flags, so a directory creation is served by the
-- watch that was set up last and has had the least time to get going.
freshWatchTrial :: Bool -> (FilePath -> IO ()) -> Int -> IO Outcome
freshWatchTrial recursive act settleMicros = withSystemTempDirectory "fsnotify-stress" $ \dir -> do
  arrivedAt <- newIORef Nothing

  let watchFn = if recursive then watchTree else watchDir

  withManager $ \mgr -> do
    stop <- watchFn mgr dir (const True) $ \_ev -> recordArrival arrivedAt

    when (settleMicros > 0) $ threadDelay settleMicros

    startedAt <- getMonotonicTime
    act (dir </> "testfile")
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

createFile' :: FilePath -> IO ()
createFile' path = writeFile path "foo"

main :: IO ()
main = do
  -- Control: one watch at a time
  report "dir-immediate" =<< replicateM 300 (freshWatchTrial False createDirectory 0)

  -- The suite's conditions: 20 watches starting at once, each acting the instant its watch is set
  -- up. On Windows a directory event is served by the second of the two watches watchDir opens,
  -- which is the one with the least time to start listening.
  report "dir-immediate-parallel20" . concat
    =<< replicateM 30 (mapConcurrently (const (freshWatchTrial False createDirectory 0)) [1 .. 20 :: Int])

  report "file-immediate-parallel20" . concat
    =<< replicateM 10 (mapConcurrently (const (freshWatchTrial False createFile' 0)) [1 .. 20 :: Int])
