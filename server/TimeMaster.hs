
module TimeMaster (timeMaster) where

import ServerMonad

import Control.Concurrent.MVar
import Data.Time.LocalTime
import System.Posix.Env

timeMaster :: TimeMasterVar -> IO ()
timeMaster tmvar
 = do (timezone, mv) <- takeMVar tmvar
      lt <- getLocalTimeInTz timezone
      putMVar mv lt
      timeMaster tmvar

getLocalTimeInTz :: String -> IO LocalTime
getLocalTimeInTz tz = do setEnv "TZ" tz True
                         tzset
                         t <- getZonedTime
                         return $ zonedTimeToLocalTime t

foreign import ccall "time.h tzset" tzset :: IO ()

