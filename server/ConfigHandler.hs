
module ConfigHandler (configHandler) where

import ServerMonad

import Builder.Config
import Builder.Utils

import GHC
import Linker
import MonadUtils
import Packages

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Dynamic
import GHC.Paths
import Prelude hiding (catch)

configHandler :: CHVar -> IO ()
configHandler chv = do m <- loadConfig
                       case m of
                           Right config -> worker chv config
                           Left err ->
                               do warn ("Can't load config:\n" ++ err)
                                  warn "Sleeping 5 seconds..."
                                  threadDelay 5000000
                                  warn "...retrying"
                                  configHandler chv

worker :: CHVar -> Config -> IO ()
worker chv config
 = do req <- takeMVar chv
      case req of
          ReloadConfig ->
              do e <- loadConfig
                 case e of
                     Left err ->
                         do warn ("Reloading config failed:\n" ++ err)
                            worker chv config
                     Right config' ->
                         worker chv config'
          GiveMeConfig mv ->
              do putMVar mv config
                 worker chv config

-- XXX Ought to catch exceptions, and return something more informative
-- than a Maybe type
loadConfig :: IO (Either String Config)
loadConfig = do
      runGhc (Just libdir) $ do
        dflags0 <- getSessionDynFlags
        let dflags1 = dflags0 {
                          hscTarget = HscInterpreted
                      }
        _ <- setSessionDynFlags dflags1

        -- Due to the global nature of the linker, if we don't unload
        -- everything then the second time we call loadConfig we get
        -- a linker failure
        (dflags2, _) <- liftIO $ initPackages dflags1
        liftIO $ unload dflags2 []

        t <- guessTarget "Config.hs" Nothing
        setTargets [t]
        ok <- load LoadAllTargets
        case ok of
            Succeeded ->
                do modGraph <- getModuleGraph
                   loadedModSummaries <- filterM (isLoaded . ms_mod_name) modGraph
                   let loadedMods = map ms_mod loadedModSummaries
                   setContext [] loadedMods
            Failed ->
                error "XXX"
        d <- dynCompileExpr "config"
        case fromDynamic d of
            Just clients -> return (Right clients)
            Nothing -> return (Left "config has wrong type")
    `catch` \e ->
        return (Left (show (e :: SomeException)))

