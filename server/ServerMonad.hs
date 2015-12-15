{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ServerMonad (
                    ServerMonad, evalServerMonad, mkServerState,
                    getUser,
                    getProtocolVersion,
                    getLastReadyTime, setLastReadyTime,
                    getUserInfo,
                    getDirectory, getNotifierVar,
                    -- XXX Don't really belong here:
                    Directory(..), MessagerRequest(..),
                    getConfig,
                    NVar,
                    CHVar, ConfigHandlerRequest(..),
                    TimeMasterVar, getLocalTimeInTz,
                    baseDir,
                    Ppr(..), Who(..), ClientThread(..), CoreThread(..),
                    verbose, verbose', warn, warn',
                   ) where

import Builder.Config
import Builder.Handlelike
import Builder.Utils

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.State
import Data.Time.Format
import Data.Time.LocalTime
import Network.Socket
#if !MIN_VERSION_time(1,5,0)
import System.Locale
#endif

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
    deriving (Functor, Applicative, Monad, MonadIO)

data ServerState = ServerState {
                       ss_handleOrSsl :: HandleOrSsl,
                       ss_user :: String,
                       ss_protocolVersion :: ProtocolVersion,
                       ss_directory :: Directory,
                       ss_last_ready_time :: TimeOfDay
                   }

mkServerState :: HandleOrSsl
              -> User
              -> ProtocolVersion
              -> Directory
              -> TimeOfDay
              -> ServerState
mkServerState h u pv directory lrt
    = ServerState {
          ss_handleOrSsl = h,
          ss_user = u,
          ss_protocolVersion = pv,
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

getProtocolVersion :: ServerMonad ProtocolVersion
getProtocolVersion = do st <- ServerMonad get
                        return $ ss_protocolVersion st

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

verbose :: String -> ServerMonad ()
verbose str = do directory <- getDirectory
                 u <- getUser
                 liftIO $ verbose' directory (ClientThread (User u)) str

warn :: String -> ServerMonad ()
warn str = do directory <- getDirectory
              u <- getUser
              liftIO $ warn' directory (ClientThread (User u)) str

verbose' :: Directory -> Who -> String -> IO ()
verbose' directory who str = message' directory Verbose who str

warn' :: Directory -> Who -> String -> IO ()
warn' directory who str = message' directory Normal who ("Warning: " ++ str)

message' :: Directory -> Verbosity -> Who -> String -> IO ()
message' directory verbosity who str
 = do lt <- getLocalTimeInTz directory "UTC"
      let fmt = "[%Y-%m-%d %H:%M:%S]"
          t = formatTime defaultTimeLocale fmt lt
      putMVar (dir_messagerVar directory)
              (Message verbosity $ unwords [t, ppr who, str])

data Who = ClientThread ClientThread
         | CoreThread CoreThread
         | AddrThread SockAddr
data ClientThread = User User
                  | Unauthed SockAddr
data CoreThread = MessagerThread
                | NotifierThread
                | ConfigThread
                | TimeThread
                | MainThread

class Ppr a where
    ppr :: a -> String

instance Ppr Who where
    ppr (ClientThread ct) = "["      ++ ppr ct ++ "]"
    ppr (CoreThread   ct) = "[core:" ++ ppr ct ++ "]"
    ppr (AddrThread   sa) = "[addr:" ++ ppr sa ++ "]"

instance Ppr ClientThread where
    ppr (User u)     = "U:" ++ u
    ppr (Unauthed a) = "unauthed:" ++ ppr a

instance Ppr CoreThread where
    ppr MessagerThread = "Messager"
    ppr NotifierThread = "Notifier"
    ppr ConfigThread   = "Config handler"
    ppr TimeThread     = "Time"
    ppr MainThread     = "Main"

instance Ppr SockAddr where
    ppr = show

getLocalTimeInTz :: Directory -> String -> IO LocalTime
getLocalTimeInTz directory tz
 = do mv <- newEmptyMVar
      putMVar (dir_timeMasterVar directory) (tz, mv)
      takeMVar mv

