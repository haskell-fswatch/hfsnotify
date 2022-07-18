module System.FSNotify.Debouncer.Helper (getPathTimeDir) where

import Data.Time (UTCTime)
import System.FSNotify.Types (Event (..), EventIsDirectory)

-- FIXME There has to be a better way to do this
getPathTimeDir :: Event -> (FilePath, UTCTime, EventIsDirectory)
getPathTimeDir (Added                   p t d)   = (p, t, d)
getPathTimeDir (Modified                p t d)   = (p, t, d)
getPathTimeDir (ModifiedAttributes      p t d)   = (p, t, d)
getPathTimeDir (Removed                 p t d)   = (p, t, d)
getPathTimeDir (WatchedDirectoryRemoved p t d)   = (p, t, d)
getPathTimeDir (CloseWrite              p t d)   = (p, t, d)
getPathTimeDir (Unknown                 p t d _) = (p, t, d)
