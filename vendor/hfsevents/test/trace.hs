{-
   simple event tracer that monitors the current
   directory and prints all events
-}

import System.OSX.FSEvents

import Data.Bits
import Data.List (unwords)
import Data.Word

main = do
  es <- eventStreamCreate ["."] 1.0 True True True trace
  getLine
  eventStreamDestroy es
  return ()

trace :: Event -> IO ()
trace e = do
  putStrLn $ "id:    " ++ show (eventId e)
  putStrLn $ "path:  " ++ eventPath e
  putStrLn $ "flags: " ++ showEventFlags (eventFlags e)
  putStrLn ""

showEventFlags :: Word64 -> String
showEventFlags fl = unwords (map fst . filter hasFlag $ flagList)
  where
    hasFlag (_,f) = fl .&. f /= 0

-- fixme come up with some better way to make this list
flagList :: [(String, Word64)]
flagList = [ ("MustScanSubDirs"   , 0x00000001)
           , ("UserDropped"       , 0x00000002)
           , ("KernelDropped"     , 0x00000004)
           , ("EventIdsWrapped"   , 0x00000008)
           , ("HistoryDone"       , 0x00000010)
           , ("RootChanged"       , 0x00000020)
           , ("Mount"             , 0x00000040)
           , ("Unmount"           , 0x00000080)
           , ("ItemCreated"       , 0x00000100)
           , ("ItemRemoved"       , 0x00000200)
           , ("ItemInodeMetaMod"  , 0x00000400)
           , ("ItemRenamed"       , 0x00000800)
           , ("ItemModified"      , 0x00001000)
           , ("ItemFinderInfoMod" , 0x00002000)
           , ("ItemChangeOwner"   , 0x00004000)
           , ("ItemXattrMod"      , 0x00008000)
           , ("ItemIsFile"        , 0x00010000)
           , ("ItemIsDir"         , 0x00020000)
           , ("ItemIsSymlink"     , 0x00040000)
           ]
