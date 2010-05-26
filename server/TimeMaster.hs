
module TimeMaster (timeMaster) where

import ServerMonad

import Control.Concurrent.MVar
import Data.Time.LocalTime
import System.Posix.Env

timeMaster :: TimeMasterVar -> IO ()
timeMaster tmvar
 = do (timezone, mv) <- takeMVar tmvar
      tod <- getTODinTZ timezone
      putMVar mv tod
      timeMaster tmvar

getTODinTZ :: String -> IO TimeOfDay
getTODinTZ tz = do setEnv "TZ" tz True
                   tzset
                   t <- getZonedTime
                   return $ localTimeOfDay $ zonedTimeToLocalTime t

foreign import ccall "time.h tzset" tzset :: IO ()

