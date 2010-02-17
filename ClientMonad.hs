
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ClientMonad (ClientMonad, evalClientMonad, mkClientState,
                    getVerbosity, getBaseDir, getHandle, setHandle
                   ) where

import Handlelike
import Utils

import Control.Monad.State
import System.IO

newtype ClientMonad a = ClientMonad (StateT ClientState IO a)
    deriving (Monad, MonadIO)

data ClientState = ClientState {
                       cs_verbosity :: Verbosity,
                       cs_basedir :: FilePath,
                       cs_handleOrSsl :: HandleOrSsl
                   }

mkClientState :: Verbosity -> FilePath -> HandleOrSsl -> ClientState
mkClientState v bd h
    = ClientState {
          cs_verbosity = v,
          cs_basedir = bd,
          cs_handleOrSsl = h
      }

evalClientMonad :: ClientMonad a -> ClientState -> IO a
evalClientMonad (ClientMonad m) cs = evalStateT m cs

getVerbosity :: ClientMonad Verbosity
getVerbosity = do st <- ClientMonad get
                  return $ cs_verbosity st

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

