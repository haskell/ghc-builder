
module ConfigHandler (configHandler) where

import ServerMonad

import Builder.Config

import DynFlags
import ErrUtils
import GHC
import Linker
import MonadUtils
import Outputable
import Packages

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Dynamic
import GHC.Paths

configHandler :: Directory -> CHVar -> IO ()
configHandler directory chv
    = do m <- loadConfig warn
         case m of
             Right config -> worker warn chv config
             Left err ->
                 do warn ("Can't load config:\n" ++ err)
                    warn "Sleeping 5 seconds..."
                    threadDelay 5000000
                    warn "...retrying"
                    configHandler directory chv
    where warn = warn' directory (CoreThread ConfigThread)

worker :: (String -> IO ()) -> CHVar -> Config -> IO ()
worker warn chv config
 = do req <- takeMVar chv
      case req of
          ReloadConfig ->
              do e <- loadConfig warn
                 case e of
                     Left err ->
                         do warn ("Reloading config failed:\n" ++ err)
                            worker warn chv config
                     Right config' ->
                         worker warn chv config'
          GiveMeConfig mv ->
              do putMVar mv config
                 worker warn chv config

loadConfig :: (String -> IO ()) -> IO (Either String Config)
loadConfig warn = do
      runGhc (Just libdir) $ do
        dflags0 <- getSessionDynFlags
        let dflags1 = dflags0 {
                          hscTarget = HscInterpreted,
                          log_action = logAction warn,
                          flushOut = FlushOut $ return (),
                          flushErr = FlushErr $ return ()
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
                   let modNames = map ms_mod_name modGraph
                   loadedModNames <- filterM isLoaded modNames
                   let imps = map (IIDecl . simpleImportDecl) loadedModNames
                   setContext imps
            Failed ->
                error "XXX"
        d <- dynCompileExpr "config"
        case fromDynamic d of
            Just clients -> return (Right clients)
            Nothing -> return (Left "config has wrong type")
    `catch` \e ->
        return (Left (show (e :: SomeException)))

logAction :: (String -> IO ()) -> LogAction
logAction warn dflags severity srcSpan _style msg
 = do let locatedMsg = mkLocMessage severity srcSpan msg
          msg' = case severity of
                 SevOutput  -> text "GHC Output:" <+> msg
                 SevDump    -> text "GHC Dump:"   <+> msg
                 SevInfo    -> text "GHC Info:"   <+> msg
                 SevFatal   -> text "GHC Fatal:"  <+> msg
                 SevWarning -> locatedMsg
                 SevError   -> locatedMsg
      warn (showSDoc dflags msg')

