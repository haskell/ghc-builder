
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ServerMonad (
                    ServerMonad, evalServerMonad, mkServerState,
                    getVerbosity, getUser,
                    getLastReadyTime, setLastReadyTime,
                    getScheduledBuildTime, getBuildInstructions,
                    getNotifierVar,
                    -- XXX Don't really belong here:
                    NVar,
                    baseDir,
                   ) where

import Handlelike
import Utils

import Control.Concurrent.MVar
import Control.Monad.State
import Data.Time.LocalTime
import System.IO

type NVar = MVar (User, BuildNum)

baseDir :: FilePath
baseDir = "data"

newtype ServerMonad a = ServerMonad (StateT ServerState IO a)
    deriving (Monad, MonadIO)

data ServerState = ServerState {
                       ss_handleOrSsl :: HandleOrSsl,
                       ss_user :: String,
                       ss_verbosity :: Verbosity,
                       ss_webpage_creation_var :: NVar,
                       ss_last_ready_time :: TimeOfDay,
                       ss_user_info :: UserInfo
                   }

mkServerState :: HandleOrSsl -> User -> Verbosity -> NVar
              -> TimeOfDay -> UserInfo
              -> ServerState
mkServerState h u v wcvar lrt ui
    = ServerState {
          ss_handleOrSsl = h,
          ss_user = u,
          ss_verbosity = v,
          ss_webpage_creation_var = wcvar,
          ss_last_ready_time = lrt,
          ss_user_info = ui
      }

evalServerMonad :: ServerMonad a -> ServerState -> IO a
evalServerMonad (ServerMonad m) cs = evalStateT m cs

getVerbosity :: ServerMonad Verbosity
getVerbosity = do st <- ServerMonad get
                  return $ ss_verbosity st

getHandleOrSsl :: ServerMonad HandleOrSsl
getHandleOrSsl = do st <- ServerMonad get
                    return $ ss_handleOrSsl st

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
                           return $ ui_buildTime $ ss_user_info st

getBuildInstructions :: ServerMonad [BuildStep]
getBuildInstructions = do st <- ServerMonad get
                          return $ ui_buildInstructions $ ss_user_info st

getNotifierVar :: ServerMonad NVar
getNotifierVar = do st <- ServerMonad get
                    return $ ss_webpage_creation_var st

instance HandlelikeM ServerMonad where
    hlPutStrLn str = do h <- getHandleOrSsl
                        liftIO $ hlPutStrLn' h str
    hlGetLine = do h <- getHandleOrSsl
                   liftIO $ hlGetLine' h
    hlGet n = do h <- getHandleOrSsl
                 liftIO $ hlGet' h n

