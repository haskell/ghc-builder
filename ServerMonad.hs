
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ServerMonad (
                    ServerMonad, evalServerMonad, mkServerState,
                    getVerbosity, getHandle, getUser,
                    getLastReadyTime, setLastReadyTime,
                    getScheduledBuildTime,
                    getWebpageCreatorVar,
                    -- XXX Don't really belong here:
                    WCVar,
                    baseDir,
                   ) where

import BuildStep
import Utils

import Control.Concurrent.MVar
import Control.Monad.State
import Data.Time.LocalTime
import System.IO

type WCVar = MVar (User, BuildNum)

baseDir :: FilePath
baseDir = "data"

newtype ServerMonad a = ServerMonad (StateT ServerState IO a)
    deriving (Monad, MonadIO)

data ServerState = ServerState {
                       ss_handle :: Handle,
                       ss_user :: String,
                       ss_verbosity :: Verbosity,
                       ss_webpage_creation_var :: WCVar,
                       ss_last_ready_time :: TimeOfDay,
                       ss_scheduled_build_time :: BuildTime
                   }

mkServerState :: Handle -> User -> Verbosity -> WCVar
              -> TimeOfDay -> BuildTime
              -> ServerState
mkServerState h u v wcvar lrt bt
    = ServerState {
          ss_handle = h,
          ss_user = u,
          ss_verbosity = v,
          ss_webpage_creation_var = wcvar,
          ss_last_ready_time = lrt,
          ss_scheduled_build_time = bt
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

getScheduledBuildTime :: ServerMonad BuildTime
getScheduledBuildTime = do st <- ServerMonad get
                           return $ ss_scheduled_build_time st

getWebpageCreatorVar :: ServerMonad WCVar
getWebpageCreatorVar = do st <- ServerMonad get
                          return $ ss_webpage_creation_var st

