
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ServerMonad (ServerMonad, evalServerMonad, mkServerState,
                    getVerbosity, getHandle, getUser,
                    getLastReadyTime, setLastReadyTime,
                    getScheduledBuildTime
                   ) where

import Data.Time.LocalTime
import Utils

import Control.Monad.State
import System.IO

newtype ServerMonad a = ServerMonad (StateT ServerState IO a)
    deriving (Monad, MonadIO)

data ServerState = ServerState {
                       ss_handle :: Handle,
                       ss_user :: String,
                       ss_verbosity :: Verbosity,
                       ss_last_ready_time :: TimeOfDay,
                       ss_scheduled_build_time :: TimeOfDay
                   }

mkServerState :: Handle -> String -> Verbosity -> TimeOfDay -> TimeOfDay
              -> ServerState
mkServerState h u v lrt sbt
    = ServerState {
          ss_handle = h,
          ss_user = u,
          ss_verbosity = v,
          ss_last_ready_time = lrt,
          ss_scheduled_build_time = sbt
      }

evalServerMonad :: ServerMonad a -> ServerState -> IO a
evalServerMonad (ServerMonad m) cs = evalStateT m cs

getVerbosity :: ServerMonad Verbosity
getVerbosity = do st <- ServerMonad get
                  return $ ss_verbosity st

getHandle :: ServerMonad Handle
getHandle = do st <- ServerMonad get
               return $ ss_handle st

getUser :: ServerMonad String
getUser = do st <- ServerMonad get
             return $ ss_user st

getLastReadyTime :: ServerMonad TimeOfDay
getLastReadyTime = do st <- ServerMonad get
                      return $ ss_last_ready_time st

setLastReadyTime :: TimeOfDay -> ServerMonad ()
setLastReadyTime tod = do st <- ServerMonad get
                          ServerMonad $ put $ st { ss_last_ready_time = tod }

getScheduledBuildTime :: ServerMonad TimeOfDay
getScheduledBuildTime = do st <- ServerMonad get
                           return $ ss_scheduled_build_time st

