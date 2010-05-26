
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ClientMonad (ClientMonad, evalClientMonad, mkClientState,
                    getVerbosity, getHost, getBaseDir, getHandle, setHandle
                   ) where

import Builder.Handlelike
import Builder.Utils

import Control.Monad.State

newtype ClientMonad a = ClientMonad (StateT ClientState IO a)
    deriving (Monad, MonadIO)

data ClientState = ClientState {
                       cs_verbosity :: Verbosity,
                       cs_host :: String,
                       cs_basedir :: FilePath,
                       cs_handleOrSsl :: HandleOrSsl
                   }

mkClientState :: Verbosity -> String -> FilePath -> HandleOrSsl -> ClientState
mkClientState v host bd h
    = ClientState {
          cs_verbosity = v,
          cs_host = host,
          cs_basedir = bd,
          cs_handleOrSsl = h
      }

evalClientMonad :: ClientMonad a -> ClientState -> IO a
evalClientMonad (ClientMonad m) cs = evalStateT m cs

getVerbosity :: ClientMonad Verbosity
getVerbosity = do st <- ClientMonad get
                  return $ cs_verbosity st

getHost :: ClientMonad String
getHost = do st <- ClientMonad get
             return $ cs_host st

getHandle :: ClientMonad HandleOrSsl
getHandle = do st <- ClientMonad get
               return $ cs_handleOrSsl st

setHandle :: HandleOrSsl -> ClientMonad ()
setHandle h = do st <- ClientMonad get
                 ClientMonad $ put $ st { cs_handleOrSsl = h }

getBaseDir :: ClientMonad FilePath
getBaseDir = do st <- ClientMonad get
                return $ cs_basedir st

instance HandlelikeM ClientMonad where
    hlPutStrLn str = do h <- getHandle
                        liftIO $ hlPutStrLn' h str
    hlGetLine = do h <- getHandle
                   liftIO $ hlGetLine' h
    hlGet n = do h <- getHandle
                 liftIO $ hlGet' h n

