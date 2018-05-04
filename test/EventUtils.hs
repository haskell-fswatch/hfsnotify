{-# LANGUAGE OverloadedStrings, ImplicitParams #-}
module EventUtils where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async hiding (poll)
import Control.Monad
import Data.IORef
import Data.List (sortBy)
import Data.Monoid
import Data.Ord (comparing)
import Prelude hiding (FilePath)
import System.Directory
import System.FSNotify
import System.FilePath
import System.IO.Unsafe
import Test.Tasty.HUnit
import Text.Printf

delay :: (?timeInterval :: Int) => IO ()
delay = threadDelay ?timeInterval

-- event patterns
data EventPattern = EventPattern
  { patFile :: FilePath
  , patName :: String
  , patPredicate :: Event -> Bool
  }

evAdded, evRemoved, evModified, evAddedOrModified :: Bool -> FilePath -> EventPattern
evAdded isDirectory path = EventPattern path "Added"
  (\x -> case x of
      Added path' _ isDir | isDirectory == isDir -> pathMatches isDirectory path path'
      _ -> False
  )
evRemoved isDirectory path = EventPattern path "Removed"
  (\x -> case x of
      Removed path' _ isDir | isDirectory == isDir -> pathMatches isDirectory path path'
      _ -> False
  )
evModified isDirectory path = EventPattern path "Modified"
  (\x ->
     case x of
       Modified path' _ isDir | isDirectory == isDir -> pathMatches isDirectory path path'
       _ -> False
  )
evAddedOrModified isDirectory path = EventPattern path "AddedOrModified"
  (\x -> case x of
      Added path' _ isDir | isDirectory == isDir -> pathMatches isDirectory path path'
      Modified path' _ isDir | isDirectory == isDir -> pathMatches isDirectory path path'
      _ -> False
  )

pathMatches True path path' = path == path' || (path <> [pathSeparator]) == path'
pathMatches False path path' = path == path'

matchEvents :: [EventPattern] -> [Event] -> Assertion
matchEvents expected actual = do
  unless (length expected == length actual) $
    assertFailure $ printf
      "Unexpected number of events.\n  Expected: %s\n  Actual: %s\n"
      (show expected)
      (show actual)
  sequence_ $ (\f -> zipWith f expected actual) $ \pat ev ->
    assertBool
      (printf "Unexpected event.\n  Expected: %s\n  Actual: %s\n"
        (show expected)
        (show actual))
      (patPredicate pat ev)

instance Show EventPattern where
  show p = printf "%s %s" (patName p) (show $ patFile p)

gatherEvents
  :: (?timeInterval :: Int)
  => Bool -- use polling?
  -> (WatchManager -> FilePath -> ActionPredicate -> Action -> IO StopListening)
     -- (^ this is the type of watchDir/watchTree)
  -> FilePath
  -> IO (Async [Event])
gatherEvents poll watch path = do
  mgr <- startManagerConf defaultConfig
    { confDebounce = NoDebounce
    , confUsePolling = poll
    , confPollInterval = 2 * 10^(5 :: Int)
    }
  eventsVar <- newIORef []
  stop <- watch mgr path (const True) (\ev -> atomicModifyIORef eventsVar (\evs -> (ev:evs, ())))
  async $ do
    delay
    stop
    reverse <$> readIORef eventsVar

expectEvents
  :: (?timeInterval :: Int)
  => Bool
  -> (WatchManager -> FilePath -> ActionPredicate -> Action -> IO StopListening)
  -> FilePath -> [EventPattern] -> IO () -> Assertion
expectEvents poll w path pats action = do
  a <- gatherEvents poll w path
  action
  evs <- wait a
  matchEvents pats $ sortBy (comparing eventTime) evs

testDirPath :: FilePath
testDirPath = (unsafePerformIO getCurrentDirectory) </> "testdir"

expectEventsHere :: (?timeInterval::Int) => Bool -> [EventPattern] -> IO () -> Assertion
expectEventsHere poll = expectEvents poll watchDir testDirPath

expectEventsHereRec :: (?timeInterval::Int) => Bool -> [EventPattern] -> IO () -> Assertion
expectEventsHereRec poll = expectEvents poll watchTree testDirPath
