
{-# LANGUAGE PatternGuards #-}

module Main where

import BuildStep
import Config
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
import System.Exit
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
                 sendHandle v ssl "200 Welcome to SSL"
                 authClient v (Ssl ssl) mv
          "NO SSL" ->
              do sendHandle v s "200 OK, no SSL"
                 authClient v (Socket s) mv
          _ -> do sendHandle v s "501 Expected SSL instructions"
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
                             sendHandle v h "200 authenticated"
                             let serverState = mkServerState
                                                   h user v mv tod ui
                             evalServerMonad handleClient serverState
                      _ ->
                          do sendHandle v h "501 auth failed"
                             authClient v h mv
                  _ ->
                      do sendHandle v h "500 I don't understand"
                         authClient v h mv
          Nothing ->
              case msg of
                  "HELP" ->
                      -- XXX
                      do sendHandle v h "500 I don't understand"
                         authClient v h mv
                  _ ->
                      do sendHandle v h "500 I don't understand"
                         authClient v h mv

verbose :: String -> ServerMonad ()
verbose str = do v <- getVerbosity
                 liftIO $ verbose' v str

verbose' :: Verbosity -> String -> IO ()
verbose' v str = when (v >= Verbose) $ putStrLn str

sendHandle :: Handlelike h => Verbosity -> h -> String -> IO ()
sendHandle v h str
 = do verbose' v ("Sending: " ++ show str)
      hlPutStrLn' h str

sendClient :: String -> ServerMonad ()
sendClient str
 = do verbose ("Sending: " ++ show str)
      hlPutStrLn str

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
                            do sendClient "201 Instructions follow"
                               user <- getUser
                               let lastBuildNumFile = baseDir </> "clients" </> user </> "last_build_num_allocated"
                               lastBuildNum <- readFromFile lastBuildNumFile
                               let thisBuildNum = lastBuildNum + 1
                               writeToFile lastBuildNumFile thisBuildNum
                               bss <- getBuildInstructions
                               sendSizedThing $ mkBuildInstructions thisBuildNum bss
                               sendClient "200 That's it"
                        "LAST UPLOADED" ->
                            do sendClient "201 Build number follows"
                               user <- getUser
                               let lastBuildNumFile = baseDir </> "clients" </> user </> "last_build_num_uploaded"
                               lastBuildNum <- readFromFile lastBuildNumFile
                               sendSizedThing (lastBuildNum :: BuildNum)
                               sendClient "200 That's it"
                        "READY" ->
                            do
                               scheduled <- getScheduledBuildTime
                               case scheduled of
                                   Continuous ->
                                       sendClient "202 Time for a build"
                                   Timed tod ->
                                       do prev <- getLastReadyTime
                                          current <- getTOD
                                          setLastReadyTime current
                                          if scheduledTimePassed prev current tod
                                              then sendClient "202 Time for a build"
                                              else sendClient "200 Nothing to do"
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
                            sendClient "500 I don't understand"
                    talk

receiveBuildStep :: BuildNum -> BuildStepNum -> ServerMonad ()
receiveBuildStep buildNum buildStepNum
 = do user <- getUser
      let userDir = baseDir </> "clients" </> user
          buildDir = userDir </> "builds" </> show buildNum
          stepsDir = buildDir </> "steps"
          buildStepDir = stepsDir </> show buildStepNum
      liftIO $ createDirectoryIfMissing False buildDir
      liftIO $ createDirectoryIfMissing False stepsDir
      liftIO $ createDirectoryIfMissing False buildStepDir
      -- Get the name
      sendClient "203 Send name"
      name <- readSizedThing
      writeBinaryFile (buildStepDir </> "name") (show (name :: String))
      -- Get the program
      sendClient "203 Send subdir"
      subdir <- readSizedThing
      writeBinaryFile (buildStepDir </> "subdir") (show (subdir :: FilePath))
      -- Get the program
      sendClient "203 Send program"
      prog <- readSizedThing
      writeBinaryFile (buildStepDir </> "prog") (show (prog :: String))
      -- Get the args
      sendClient "203 Send args"
      args <- readSizedThing
      writeBinaryFile (buildStepDir </> "args") (show (args :: [String]))
      -- Get the exit code
      sendClient "203 Send exit code"
      ec <- readSizedThing
      writeBinaryFile (buildStepDir </> "exitcode") (show (ec :: ExitCode))
      -- Get the output
      sendClient "203 Send output"
      output <- getSizedThing
      writeBinaryFile (buildStepDir </> "output") output
      -- and tell the client that we're done, so it can delete its copy
      -- of the files
      sendClient "200 Got it, thanks!"

receiveBuildResult :: BuildNum -> ServerMonad ()
receiveBuildResult buildNum
 = do user <- getUser
      let userDir = baseDir </> "clients" </> user
          buildDir = userDir </> "builds" </> show buildNum
      liftIO $ createDirectoryIfMissing False buildDir
      -- Get the program
      sendClient "203 Send result"
      res <- readSizedThing
      writeBinaryFile (buildDir </> "result") (show (res :: Result))
      -- update the "last buildnum uploaded" record
      let lastFile = userDir </> "last_build_num_uploaded"
      l <- readFromFile lastFile
      when (buildNum > l) $ writeToFile lastFile buildNum
      -- and tell the client that we're done, so it can delete its copy
      -- of the files
      mv <- getWebpageCreatorVar
      liftIO $ putMVar mv (user, buildNum)
      sendClient "200 Got it, thanks!"

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

