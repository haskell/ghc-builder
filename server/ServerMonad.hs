
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ServerMonad (
                    ServerMonad, evalServerMonad, mkServerState,
                    getUser,
                    getLastReadyTime, setLastReadyTime,
                    getUserInfo,
                    getDirectory, getNotifierVar,
                    -- XXX Don't really belong here:
                    Directory(..), MessagerRequest(..),
                    getConfig,
                    NVar,
                    CHVar, ConfigHandlerRequest(..),
                    TimeMasterVar,
                    baseDir,
                   ) where

import Builder.Handlelike
import Builder.Utils

import Control.Concurrent.MVar
import Control.Monad.State
import Data.Time.LocalTime

type NVar = MVar (User, BuildNum)

type CHVar = MVar ConfigHandlerRequest

type MessagerVar = MVar MessagerRequest

type TimeMasterVar = MVar (String, MVar LocalTime)

data ConfigHandlerRequest = ReloadConfig
                          | GiveMeConfig (MVar Config)

data MessagerRequest = Message Verbosity String
                     | Reopen

baseDir :: FilePath
baseDir = "data"

newtype ServerMonad a = ServerMonad (StateT ServerState IO a)
    deriving (Monad, MonadIO)

data ServerState = ServerState {
                       ss_handleOrSsl :: HandleOrSsl,
                       ss_user :: String,
                       ss_directory :: Directory,
                       ss_last_ready_time :: TimeOfDay
                   }

mkServerState :: HandleOrSsl -> User -> Directory -> TimeOfDay
              -> ServerState
mkServerState h u directory lrt
    = ServerState {
          ss_handleOrSsl = h,
          ss_user = u,
          ss_directory = directory,
          ss_last_ready_time = lrt
      }

evalServerMonad :: ServerMonad a -> ServerState -> IO a
evalServerMonad (ServerMonad m) cs = evalStateT m cs

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

getUserInfo :: ServerMonad (Maybe UserInfo)
getUserInfo = do st <- ServerMonad get
                 config <- liftIO $ getConfig (ss_directory st)
                 return $ lookup (ss_user st) $ config_clients config

getDirectory :: ServerMonad Directory
getDirectory = do st <- ServerMonad get
                  return $ ss_directory st

getNotifierVar :: ServerMonad NVar
getNotifierVar = liftM dir_notifierVar $ getDirectory

instance HandlelikeM ServerMonad where
    hlPutStrLn str = do h <- getHandleOrSsl
                        liftIO $ hlPutStrLn' h str
    hlGetLine = do h <- getHandleOrSsl
                   liftIO $ hlGetLine' h
    hlGet n = do h <- getHandleOrSsl
                 liftIO $ hlGet' h n

data Directory = Directory {
                     dir_messagerVar :: MessagerVar,
                     dir_notifierVar :: NVar,
                     dir_configHandlerVar :: CHVar,
                     dir_timeMasterVar :: TimeMasterVar
                 }

getConfig :: Directory -> IO Config
getConfig directory
 = do mv <- newEmptyMVar
      putMVar (dir_configHandlerVar directory) (GiveMeConfig mv)
      takeMVar mv

