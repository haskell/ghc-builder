
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ClientMonad (ClientMonad, evalClientMonad, mkClientState,
                    getVerbosity, getBaseDir
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
                       cs_handle :: Handle
                   }

mkClientState :: Verbosity -> FilePath -> Handle -> ClientState
mkClientState v bd h
    = ClientState {
          cs_verbosity = v,
          cs_basedir = bd,
          cs_handle = h
      }

evalClientMonad :: ClientMonad a -> ClientState -> IO a
evalClientMonad (ClientMonad m) cs = evalStateT m cs

getVerbosity :: ClientMonad Verbosity
getVerbosity = do st <- ClientMonad get
                  return $ cs_verbosity st

getHandle :: ClientMonad Handle
getHandle = do st <- ClientMonad get
               return $ cs_handle st

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

