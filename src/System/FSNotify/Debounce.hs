

-- | implementation for debounce operations
module System.FSNotify.Debounce 
    ( wrapDebounce
    ) where

import Data.IORef
import Data.Time (NominalDiffTime, diffUTCTime)
--import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Control.Monad (join)
import System.FSNotify.Types
import System.FSNotify.Path (fp)

-- | wrap an event handler with a debounce action. The given epsilon
-- is checked against the event times.
wrapDebounce :: NominalDiffTime -> (Event -> IO ()) -> IO (Event -> IO ())
wrapDebounce dt action | dt > 0 = withDebounce dt action `fmap` newIORef eventDefault
wrapDebounce _ action = return action

-- debounce an event
withDebounce :: NominalDiffTime -> (Event -> IO ()) -> IORef Event -> Event -> IO ()
withDebounce dt action rfLast evNew = join $ atomicModifyIORef rfLast dbAction where
    dbAction evOld = if bounces dt evOld evNew then (evOld, return ())
                                               else (evNew, action evNew)
-- | The default event that provides a basis for comparison.
eventDefault :: Event
eventDefault = Event Rem (fp "") (posixSecondsToUTCTime 0) False

-- | A predicate indicating whether two events may be considered "the same
-- event". This predicate is applied to the most recent dispatched event and
-- the current event after the client-specified ActionPredicate is applied,
-- before the event is dispatched.
bounces :: NominalDiffTime -> Event -> Event -> Bool
bounces epsilon e1 e2 = (isExistsEvent e1 == isExistsEvent e2) 
                     && (eventPath e1 == eventPath e2)
                     && (timeDiff < epsilon)
    where timeDiff = abs $ diffUTCTime (eventTime e2) (eventTime e1)

