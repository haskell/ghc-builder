
module TimeMaster (timeMaster) where

import ServerMonad

import Control.Concurrent.MVar
import Data.Time.LocalTime
import System.Posix.Env

timeMaster :: TimeMasterVar -> IO ()
timeMaster tmvar
 = do (timezone, mv) <- takeMVar tmvar
      lt <- getLocalTimeInTimezone timezone
      putMVar mv lt
      timeMaster tmvar

getLocalTimeInTimezone :: String -> IO LocalTime
getLocalTimeInTimezone tz = do setEnv "TZ" tz True
                               tzset
                               t <- getZonedTime
                               return $ zonedTimeToLocalTime t

foreign import ccall "time.h tzset" tzset :: IO ()

