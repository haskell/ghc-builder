
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ClientMonad where

import Control.Monad.State
import System.IO

newtype ClientMonad a = ClientMonad (StateT ClientState IO a)
    deriving (Monad, MonadIO)

data ClientState = ClientState {
                       cs_basedir :: FilePath,
                       cs_handle :: Handle
                   }

evalClientMonad :: ClientMonad a -> ClientState -> IO a
evalClientMonad (ClientMonad m) cs = evalStateT m cs

getHandle :: ClientMonad Handle
getHandle = do st <- ClientMonad get
               return $ cs_handle st

getBaseDir :: ClientMonad FilePath
getBaseDir = do st <- ClientMonad get
                return $ cs_basedir st

