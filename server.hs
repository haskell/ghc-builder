
{-# LANGUAGE PatternGuards #-}

module Main where

import BuildStep
import Config
import Files
import Handlelike
import ServerMonad
import Utils
import WebpageCreation

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.List
import Data.Time.LocalTime
import Network.Socket
import OpenSSL
import OpenSSL.PEM
import OpenSSL.Session
-- import OpenSSL.X509
import Prelude hiding (catch)
import System.Directory
import System.Environment
import System.FilePath
import System.IO

main :: IO ()
main = do args <- getArgs
          case args of
              []              -> withSocketsDo $ withOpenSSL $ runServer Normal
              ["-v"]          -> withSocketsDo $ withOpenSSL $ runServer Verbose
              ["init"]        -> initServer
              ["add", client] -> addClient client
              _               -> die "Bad args"

initServer :: IO ()
initServer = do -- XXX We really ought to catch an already-exists
                -- exception and handle it properly
                createDirectory baseDir
                createDirectory (baseDir </> "clients")

addClient :: String -> IO ()
addClient client
 | null client = die "Null client name!"
 | not (all isAlpha client) = die "Bad client name"
 | otherwise = do -- XXX We really ought to catch an already-exists
                  -- exception and handle it properly
                  let clientDir = baseDir </> "clients" </> client
                  createDirectory clientDir
                  createDirectory (clientDir </> "builds")
                  writeToFile (clientDir </> "last_build_num_uploaded")
                              (0 :: BuildNum)
                  writeToFile (clientDir </> "last_build_num_allocated")
                              (0 :: BuildNum)
                  createDirectory (baseDir </> "web/builders" </> client)
                  putStrLn "OK, client added"

runServer :: Verbosity -> IO ()
runServer v =
    do webpageCreationVar <- newEmptyMVar
       let webpageCreatorThread
               = webpageCreator webpageCreationVar
                 `catch` \e ->
                     do verbose' v ("Webpage creation thread got an exception:\n" ++ show (e :: SomeException) ++ "\nRestarting...")
                        webpageCreatorThread
       forkIO webpageCreatorThread
       addrinfos <- getAddrInfo Nothing Nothing (Just (show port))
       let serveraddr = head addrinfos
       bracket (socket (addrFamily serveraddr) Stream defaultProtocol)
               sClose
               (listenForClients v webpageCreationVar serveraddr)

listenForClients :: Verbosity -> WCVar -> AddrInfo -> Socket
                 -> IO ()
listenForClients v mv serveraddr sock
 = do bindSocket sock (addrAddress serveraddr)
      listen sock 1
      let mainLoop = do (conn, _) <- Network.Socket.accept sock
                        forkIO $ startSsl v conn mv
                        mainLoop
      mainLoop

startSsl :: Verbosity -> Socket -> WCVar -> IO ()
startSsl v s mv
 = do msg <- hlGetLine' s
      when (v >= Verbose) $ putStrLn ("Received: " ++ show msg)
      case msg of
          "START SSL" ->
              do -- rootPem <- readFile "root.pem"
                 serverPem <- readFile "server.pem"
                 -- rootX509 <- readX509 rootPem
                 serverX509 <- readX509 serverPem
                 serverPrivateKey <- readPrivateKey serverPem (PwStr "password")
                 -- verf <- verifyX509 rootX509 serverPrivateKey
                 sslContext <- context
                 contextSetCertificate sslContext serverX509
                 contextSetPrivateKey sslContext serverPrivateKey
                 -- contextCheckPrivateKey sslContext
                 contextSetCAFile sslContext "root.pem"
                 ssl <- OpenSSL.Session.connection sslContext s
                 OpenSSL.Session.accept ssl
                 -- mpc <- getPeerCertificate ssl
                 -- getVerifyResult ssl
                 sendHandle v ssl respOK "Welcome to SSL"
                 authClient v (Ssl ssl) mv
          "NO SSL" ->
              do sendHandle v s respOK "OK, no SSL"
                 authClient v (Socket s) mv
          _ -> do sendHandle v s respHuh "Expected SSL instructions"
                  startSsl v s mv

authClient :: Verbosity -> HandleOrSsl -> WCVar -> IO ()
authClient v h mv
 = do msg <- hlGetLine' h
      when (v >= Verbose) $ putStrLn ("Received: " ++ show msg)
      case stripPrefix "AUTH " msg of
          Just xs ->
              case break (' ' ==) xs of
                  (user, ' ' : pass) ->
                      case lookup user clients of
                      Just ui
                       | ui_password ui == pass ->
                          do tod <- getTOD
                             sendHandle v h respOK "authenticated"
                             let serverState = mkServerState
                                                   h user v mv tod ui
                             evalServerMonad handleClient serverState
                      _ ->
                          do sendHandle v h respAuthFailed "auth failed"
                             authClient v h mv
                  _ ->
                      do sendHandle v h respHuh "I don't understand"
                         authClient v h mv
          Nothing ->
              case msg of
                  "HELP" ->
                      -- XXX
                      do sendHandle v h respHuh "I don't understand"
                         authClient v h mv
                  _ ->
                      do sendHandle v h respHuh "I don't understand"
                         authClient v h mv

verbose :: String -> ServerMonad ()
verbose str = do v <- getVerbosity
                 liftIO $ verbose' v str

verbose' :: Verbosity -> String -> IO ()
verbose' v str = when (v >= Verbose) $ putStrLn str

sendHandle :: Handlelike h => Verbosity -> h -> Response -> String -> IO ()
sendHandle v h resp str
 = do let respStr = show resp ++ " " ++ str
      verbose' v ("Sending: " ++ show respStr)
      hlPutStrLn' h respStr

sendClient :: Response -> String -> ServerMonad ()
sendClient resp str
 = do let respStr = show resp ++ " " ++ str
      verbose ("Sending: " ++ show respStr)
      hlPutStrLn respStr

handleClient :: ServerMonad ()
handleClient = do talk
                  handleClient
    where talk :: ServerMonad ()
          talk = do v <- getVerbosity
                    msg <- hlGetLine
                    liftIO $ when (v >= Verbose) $
                        putStrLn ("Received: " ++ show msg)
                    case msg of
                        -- XXX "HELP"
                        "BUILD INSTRUCTIONS" ->
                            do sendClient respSendSizedThing "What sort?"
                               instructions <- readSizedThing
                               user <- getUser
                               let lastBuildNumFile = baseDir </> "clients" </> user </> "last_build_num_allocated"
                               lastBuildNum <- readFromFile lastBuildNumFile
                               let thisBuildNum = lastBuildNum + 1
                               writeToFile lastBuildNumFile thisBuildNum
                               bss <- case instructions of
                                      StartBuild _ ->
                                          getBuildInstructions
                                      Idle ->
                                          -- XXX The client did something
                                          -- odd if we get here
                                          getBuildInstructions
                               sendClient respSizedThingFollows "Instructions follow"
                               sendSizedThing $ mkBuildInstructions thisBuildNum bss
                               sendClient respOK "That's it"
                        "LAST UPLOADED" ->
                            do sendClient respSizedThingFollows "Build number follows"
                               user <- getUser
                               let lastBuildNumFile = baseDir </> "clients" </> user </> "last_build_num_uploaded"
                               lastBuildNum <- readFromFile lastBuildNumFile
                               sendSizedThing (lastBuildNum :: BuildNum)
                               sendClient respOK "That's it"
                        "RESET TIME" ->
                            do current <- getTOD
                               setLastReadyTime current
                               sendClient respOK "Done"
                        "READY" ->
                            do
                               scheduled <- getScheduledBuildTime
                               what <- case scheduled of
                                       Continuous ->
                                           return (StartBuild Continuous)
                                       Timed tod ->
                                           do prev <- getLastReadyTime
                                              current <- getTOD
                                              setLastReadyTime current
                                              if scheduledTimePassed prev current tod
                                                  then return (StartBuild scheduled)
                                                  else return Idle
                               sendClient respSizedThingFollows "Your mission, should you choose to accept it, is to:"
                               sendSizedThing what
                               sendClient respOK "Off you go"
                        _
                         | Just xs <- stripPrefix "UPLOAD " msg,
                           (ys, ' ' : zs) <- break (' ' ==) xs,
                           Just buildNum <- maybeRead ys,
                           Just buildStepNum <- maybeRead zs ->
                            receiveBuildStep buildNum buildStepNum
                         | Just xs <- stripPrefix "RESULT " msg,
                           Just buildNum <- maybeRead xs ->
                            receiveBuildResult buildNum
                         | otherwise ->
                            sendClient respHuh "I don't understand"
                    talk

receiveBuildStep :: BuildNum -> BuildStepNum -> ServerMonad ()
receiveBuildStep buildNum buildStepNum
 = do user <- getUser
      let root = Server (baseDir </> "clients") user
          userDir = baseDir </> "clients" </> user
          buildDir = userDir </> "builds" </> show buildNum
          stepsDir = buildDir </> "steps"
          buildStepDir = stepsDir </> show buildStepNum
      liftIO $ createDirectoryIfMissing False buildDir
      liftIO $ createDirectoryIfMissing False stepsDir
      liftIO $ createDirectoryIfMissing False buildStepDir
      -- Get the name
      sendClient respSendSizedThing "Send name"
      mname <- getMaybeSizedThing
      putMaybeBuildStepName root buildNum buildStepNum mname
      -- Get the subdir
      sendClient respSendSizedThing "Send subdir"
      msubdir <- getMaybeSizedThing
      putMaybeBuildStepSubdir root buildNum buildStepNum msubdir
      -- Get the program
      sendClient respSendSizedThing "Send program"
      mprog <- getMaybeSizedThing
      putMaybeBuildStepProg root buildNum buildStepNum mprog
      -- Get the args
      sendClient respSendSizedThing "Send args"
      margs <- getMaybeSizedThing
      putMaybeBuildStepArgs root buildNum buildStepNum margs
      -- Get the exit code
      sendClient respSendSizedThing "Send exit code"
      mec <- getMaybeSizedThing
      putMaybeBuildStepExitcode root buildNum buildStepNum mec
      -- Get the output
      sendClient respSendSizedThing "Send output"
      moutput <- getMaybeSizedThing
      putMaybeBuildStepOutput root buildNum buildStepNum moutput
      -- and tell the client that we're done, so it can delete its copy
      -- of the files
      sendClient respOK "Got it, thanks!"

receiveBuildResult :: BuildNum -> ServerMonad ()
receiveBuildResult buildNum
 = do user <- getUser
      let root = Server (baseDir </> "clients") user
          userDir = baseDir </> "clients" </> user
          buildDir = userDir </> "builds" </> show buildNum
      liftIO $ createDirectoryIfMissing False buildDir
      -- Get the program
      sendClient respSendSizedThing "Send result"
      mres <- getMaybeSizedThing
      putMaybeBuildResult root buildNum mres
      -- update the "last buildnum uploaded" record
      let lastFile = userDir </> "last_build_num_uploaded"
      l <- readFromFile lastFile
      when (buildNum > l) $ writeToFile lastFile buildNum
      -- and tell the client that we're done, so it can delete its copy
      -- of the files
      mv <- getWebpageCreatorVar
      liftIO $ putMVar mv (user, buildNum)
      sendClient respOK "Got it, thanks!"

mkBuildInstructions :: BuildNum -> [BuildStep] -> BuildInstructions
mkBuildInstructions bn buildSteps = (bn, zip [1..] buildSteps)

scheduledTimePassed :: TimeOfDay -> TimeOfDay -> TimeOfDay -> Bool
scheduledTimePassed lastTime curTime scheduledTime
    = (lastTime < scheduledTime) &&
      (-- the simple case, where the time has passed:
       (curTime >= scheduledTime) ||
       -- the tricky case, where the time has passed,
       -- but then the clock has wrapped:
       (curTime < lastTime))

