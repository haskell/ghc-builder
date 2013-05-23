
{-# LANGUAGE RankNTypes #-}

module Main where

import qualified BuildSteps_0_1 as BS_0_1
import ConfigHandler
import Messager
import Notification
import ServerMonad
import TimeMaster

import Builder.BuildSteps
import Builder.Config
import Builder.Files
import Builder.Handlelike
import Builder.Utils

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.List
import Data.Maybe
import Data.Time.LocalTime
import Network.Socket
import OpenSSL
import OpenSSL.PEM
import OpenSSL.Session
import OpenSSL.X509
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Process
import System.Posix.Signals

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          hSetBuffering stderr LineBuffering
          args <- getArgs
          case args of
              []              -> runServer Nothing              Normal
              ["-v"]          -> runDaemon (Just "builder.log") Verbose
              ["-d", "-v"]    -> runServer Nothing              Verbose
              ["-o", o]       -> runServer (Just o)             Normal
              ["-v", "-o", o] -> runServer (Just o)             Verbose
              ["init"]        -> initServer
              ["add", client] -> addClient client
              _               -> die "Bad args"

initServer :: IO ()
initServer = do -- XXX We really ought to catch an already-exists
                -- exception and handle it properly
                createDirectory baseDir
                createDirectory (baseDir </> "clients")
                createDirectory (baseDir </> "web")
                createDirectory (baseDir </> "web" </> "builders")

addClient :: String -> IO ()
addClient client
 | null client = die "Null client name!"
 | not (isAlpha (head client)) = die "Bad client name"
 | not (all isOKChar client) = die "Bad client name"
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
    where isOKChar c = isAlphaNum c || c == '-' || c == '_'

runDaemon :: Maybe FilePath -> Verbosity -> IO ()
runDaemon mfp v
    = do _ <- forkProcess $ do
                  _ <- createSession
                  _ <- forkProcess $ do
                           hClose stdin
                           hClose stdout
                           hClose stderr
                           runServer mfp v
                  return ()
         return ()

runServer :: Maybe FilePath -> Verbosity -> IO ()
runServer mfp v = withSocketsDo $ withOpenSSL $ do
    tid <- myThreadId
    messagerVar <- newEmptyMVar
    notifierVar <- newEmptyMVar
    configHandlerVar <- newEmptyMVar
    timeMasterVar <- newEmptyMVar
    let directory = Directory {
                        dir_messagerVar = messagerVar,
                        dir_notifierVar = notifierVar,
                        dir_configHandlerVar = configHandlerVar,
                        dir_timeMasterVar = timeMasterVar
                    }
        mfph = case mfp of
               Nothing -> Nothing
               Just fp -> Just (fp, Nothing)
        mkThread t = runThread directory tid (CoreThread t)
    mkThread MessagerThread (messager directory mfph v)
    mkThread NotifierThread (notifier directory)
    mkThread ConfigThread   (configHandler directory configHandlerVar)
    mkThread TimeThread     (timeMaster timeMasterVar)
    _ <- installHandler sigHUP
                        (Catch (gotSigHUP directory))
                        Nothing
    addrinfos <- getAddrInfo Nothing (Just "0.0.0.0") (Just (show port))
    let serveraddr = head addrinfos
    bracket (socket (addrFamily serveraddr) Stream defaultProtocol)
            sClose
            (listenForClients tid directory serveraddr)

gotSigHUP :: Directory -> IO ()
gotSigHUP directory
 = do verbose' directory (CoreThread MainThread) "Reloading config"
      putMVar (dir_messagerVar directory) Reopen
      putMVar (dir_configHandlerVar directory) ReloadConfig

runThread :: Directory -> ThreadId -> Who -> IO () -> IO ()
runThread directory mainThread who f
 = do let thread :: (forall a . IO a -> IO a) -> IO ()
          thread unmask = do restart <- do unmask f
                                           return False
                                        `catches` [Handler asyncHandler,
                                                   Handler anyHandler]
                             when restart $ thread unmask
      _ <- forkIOWithUnmask thread
      return ()
    where warn = warn' directory who
          exceptionMsg e ms
           = unlines ([ppr who ++ " thread got an exception:", show e] ++ ms)
          asyncHandler :: AsyncException -> IO Bool
          asyncHandler e = do
              case who of
                  CoreThread MessagerThread ->
                      -- Can't easily write a message if it was the
                      -- messager thread that got the async exception,
                      -- but it isn't too important so we just don't
                      -- bother
                      return ()
                  _ ->
                      do warn ("Got an asynchronous exception: " ++
                               show (e :: AsyncException))
                         -- We want the above to get printed, but we are
                         -- about to terminate the program, so we need
                         -- to wait until the message thread has printed
                         -- it.

                         -- If we can put a second message, then the
                         -- messager must have picked up the first
                         -- one...
                         warn "Throwing to main"
                         -- ...and if we can put a third message, then
                         -- it must have printed the first and picked up
                         -- the second...
                         warn "Bye"
                         -- ...so now the first at least must have been
                         -- printed.
              throwTo mainThread e
              return False

          anyHandler :: SomeException -> IO Bool
          anyHandler e =
              case who of
                  CoreThread ct ->
                      do case ct of
                             MessagerThread ->
                                 -- If the messager thread died then we
                                 -- can't print a message saying what
                                 -- happened very easily. The messager
                                 -- will print a message when it
                                 -- restarts at least, so it's not too
                                 -- important.
                                 return ()
                             _ ->
                                 warn (exceptionMsg e ["Restarting..."])
                         return True
                  _ ->
                      do warn (exceptionMsg e [])
                         return False

listenForClients :: ThreadId -> Directory -> AddrInfo -> Socket -> IO ()
listenForClients tid directory serveraddr sock
 = do bindSocket sock (addrAddress serveraddr)
      listen sock 1
      let defaultProtocolVersion = 0.1
          mainLoop = do (conn, addr) <- Network.Socket.accept sock
                        let who = ClientThread (Unauthed addr)
                        verbose' directory who "Connection established"
                        _ <- runThread directory tid (AddrThread addr) $
                             startSsl defaultProtocolVersion directory who conn
                        mainLoop
      verbose' directory (CoreThread MainThread) "Server listening"
      mainLoop

fpServerPem :: FilePath
fpServerPem = "certs/server.pem"

fpRootPem :: FilePath
fpRootPem = "certs/root.pem"

startSsl :: ProtocolVersion -> Directory -> Who -> Socket -> IO ()
startSsl pv directory who s
 = do msg <- hlGetLine' s
      verbose' directory who ("Received: " ++ show msg)
      case stripPrefix "PROTO " msg of
          Just verStr ->
              case maybeRead verStr of
                  Just pv'
                   | pv' `elem` [0.1, 0.2] ->
                      do sendHandle directory s who respOK "Protocol version OK"
                         startSsl pv' directory who s
                  _ ->
                      do sendHandle directory s who respHuh "Protocol version not recognised"
                         startSsl pv directory who s
          Nothing ->
              case msg of
                  "START SSL" ->
                      do serverPem <- readFile fpServerPem
                         serverX509 <- readX509 serverPem
                         serverPrivateKey <- readPrivateKey serverPem (PwStr "password")
                         sslContext <- context
                         contextSetCertificate sslContext serverX509
                         contextSetPrivateKey sslContext serverPrivateKey
                         contextSetCAFile sslContext fpRootPem
                         ssl <- OpenSSL.Session.connection sslContext s
                         OpenSSL.Session.accept ssl
                         mUser <- verifySsl directory ssl
                         sendHandle directory ssl who respOK "Welcome to SSL"
                         authClient pv directory (Ssl ssl) who mUser
                  "NO SSL" ->
                      do sendHandle directory s who respOK "OK, no SSL"
                         authClient pv directory (Socket s) who Nothing
                  _ -> do sendHandle directory s who respHuh "Expected protocol version or SSL instructions"
                          startSsl pv directory who s

verifySsl :: Directory -> SSL -> IO (Maybe User)
verifySsl directory ssl
 = do verified <- getVerifyResult ssl
      unless verified $ do warn "Certificate doesn't verify"
                           exitFailure
      mPeerCert <- getPeerCertificate ssl
      case mPeerCert of
          Nothing ->
              -- XXX We're hitting this branch. Is that expected?
              -- do warn "No peer certificate"
              --    exitFailure
              return Nothing
          Just peerCert ->
              do mapping <- getSubjectName peerCert False
                 case lookup "CN" mapping of
                     Just user ->
                         return (Just user)
                     Nothing ->
                         do warn "Certificate has no CN"
                            exitFailure
    where warn = warn' directory (CoreThread MainThread)

authClient :: ProtocolVersion -> Directory -> HandleOrSsl -> Who -> Maybe User
           -> IO ()
authClient pv directory h who mu
 = do msg <- hlGetLine' h
      verbose' directory who ("Received: " ++ show msg)
      config <- getConfig directory
      case stripPrefix "AUTH " msg of
          Just xs ->
              case break (' ' ==) xs of
                  (user, ' ' : pass) ->
                      case lookup user (config_clients config) of
                      Nothing ->
                          authFailed ("User " ++ show user ++ " unknown")
                      Just ui
                       | ui_password ui /= pass ->
                          authFailed "Wrong password"
                       | isJust mu && (Just user /= mu) ->
                          authFailed "User doesn't match SSL certificate CN"
                       | otherwise ->
                          do tod <- getTodInTz directory (ui_timezone ui)
                             sendHandle directory h who respOK "authenticated"
                             let serverState = mkServerState
                                                   h user pv directory tod
                             evalServerMonad handleClient serverState
                                 `finally`
                                 verbose' directory (ClientThread (User user)) "Disconnected"
                  _ ->
                      do sendHandle directory h who respHuh "I don't understand"
                         authClient pv directory h who mu
          Nothing ->
              case msg of
                  "HELP" ->
                      -- XXX
                      do sendHandle directory h who respHuh "I don't understand"
                         authClient pv directory h who mu
                  _ ->
                      do sendHandle directory h who respHuh "I don't understand"
                         authClient pv directory h who mu
    where authFailed reason = do verbose' directory who ("Auth failed: " ++ reason)
                                 sendHandle directory h who respAuthFailed "auth failed"
                                 authClient pv directory h who mu

sendHandle :: Handlelike h
           => Directory -> h -> Who -> Response -> String -> IO ()
sendHandle directory h who resp str
 = do let respStr = show resp ++ " " ++ str
      verbose' directory who ("Sending: " ++ show respStr)
      hlPutStrLn' h respStr

sendClient :: Response -> String -> ServerMonad ()
sendClient resp str
 = do let respStr = show resp ++ " " ++ str
      verbose ("Sending: " ++ show respStr)
      hlPutStrLn respStr

handleClient :: ServerMonad ()
handleClient = do msg <- hlGetLine
                  verbose ("Received: " ++ show msg)
                  case msg of
                      -- XXX "HELP"
                      "BUILD INSTRUCTIONS" ->
                          withUserInfo answerBuildInstructions
                      "LAST UPLOADED" ->
                          answerLastUploaded
                      "RESET TIME" ->
                          withUserInfo answerResetTime
                      "READY" ->
                          withUserInfo answerReady
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
                  handleClient

withUserInfo :: (UserInfo -> ServerMonad ()) -> ServerMonad ()
withUserInfo f
 = do mui <- getUserInfo
      case mui of
          Nothing ->
              sendClient respIForgotYou "I forgot you"
          Just ui ->
              f ui

answerBuildInstructions :: UserInfo -> ServerMonad ()
answerBuildInstructions ui
 = do sendClient respSendSizedThing "What sort?"
      instructions <- readSizedThing
      user <- getUser
      let lastBuildNumFile = baseDir </> "clients" </> user </> "last_build_num_allocated"
      lastBuildNum <- readFromFile lastBuildNumFile
      let thisBuildNum = lastBuildNum + 1
      writeToFile lastBuildNumFile thisBuildNum
      let bss = case instructions of
                StartBuild _ ->
                    ui_buildInstructions ui
                Idle ->
                    -- XXX The client did something odd if we get here
                    ui_buildInstructions ui
      pv <- getProtocolVersion
      let buildInstructions = mkBuildInstructions instructions thisBuildNum bss
      sendClient respSizedThingFollows "Instructions follow"
      case pv of
          0.1 -> sendSizedThing $ BS_0_1.fromCurrent buildInstructions
          _   -> sendSizedThing buildInstructions
      sendClient respOK "That's it"

answerLastUploaded :: ServerMonad ()
answerLastUploaded
 = do sendClient respSizedThingFollows "Build number follows"
      user <- getUser
      let lastBuildNumFile = baseDir </> "clients" </> user </> "last_build_num_uploaded"
      lastBuildNum <- readFromFile lastBuildNumFile
      sendSizedThing (lastBuildNum :: BuildNum)
      sendClient respOK "That's it"

answerResetTime :: UserInfo -> ServerMonad ()
answerResetTime ui = do let tz = ui_timezone ui
                        directory <- getDirectory
                        current <- liftIO $ getTodInTz directory tz
                        setLastReadyTime current
                        sendClient respOK "Done"

answerReady :: UserInfo -> ServerMonad ()
answerReady ui
 = do let scheduled = ui_buildTime ui
      what <- case scheduled of
              Other why ->
                  return (StartBuild (Other why))
              Continuous ->
                  return (StartBuild Continuous)
              Timed tod ->
                  do prev <- getLastReadyTime
                     let tz = ui_timezone ui
                     directory <- getDirectory
                     current <- liftIO $ getTodInTz directory tz
                     setLastReadyTime current
                     if scheduledTimePassed prev current tod
                         then return (StartBuild scheduled)
                         else return Idle
              NoBuilds ->
                  return Idle
      sendClient respSizedThingFollows
                 "Your mission, should you choose to accept it, is to:"
      sendSizedThing what
      verbose ("Sent instructions: " ++ show what)
      sendClient respOK "Off you go"

receiveBuildStep :: BuildNum -> BuildStepNum -> ServerMonad ()
receiveBuildStep buildNum buildStepNum
 = do user <- getUser
      pv <- getProtocolVersion
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
      unless (pv == 0.1) $ do
          -- Get the mailOutput
          sendClient respSendSizedThing "Send mailOutput"
          mMailOutput <- getMaybeSizedThing
          putMaybeBuildStepMailOutput root buildNum buildStepNum mMailOutput
          -- Get the start time
          sendClient respSendSizedThing "Send start time"
          mStartTime <- getMaybeSizedThing
          putMaybeBuildStepStartTime root buildNum buildStepNum mStartTime
          -- Get the end time
          sendClient respSendSizedThing "Send end time"
          mEndTime <- getMaybeSizedThing
          putMaybeBuildStepEndTime root buildNum buildStepNum mEndTime
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
      -- Get the result
      sendClient respSendSizedThing "Send instructions"
      minstrs <- getMaybeSizedThing
      putMaybeBuildInstructions root buildNum minstrs
      sendClient respSendSizedThing "Send result"
      mres <- getMaybeSizedThing
      putMaybeBuildResult root buildNum mres
      -- update the "last buildnum uploaded" record
      let lastFile = userDir </> "last_build_num_uploaded"
      l <- readFromFile lastFile
      when (buildNum > l) $ writeToFile lastFile buildNum
      -- and tell the client that we're done, so it can delete its copy
      -- of the files
      nv <- getNotifierVar
      liftIO $ putMVar nv (user, buildNum)
      sendClient respOK "Got it, thanks!"

mkBuildInstructions :: Instructions -> BuildNum -> [BuildStep] -> BuildInstructions
mkBuildInstructions instructions bn buildSteps
 = BuildInstructions {
       bi_instructions = instructions,
       bi_buildNum = bn,
       bi_buildSteps = zip [1..] buildSteps
   }

scheduledTimePassed :: TimeOfDay -> TimeOfDay -> TimeOfDay -> Bool
scheduledTimePassed lastTime curTime scheduledTime
      -- the simple case, where the time has passed:
    = ((lastTime < scheduledTime) && (scheduledTime <= curTime))
      -- the time has passed, but then the clock has wrapped:
   || ((lastTime < scheduledTime) && (curTime < lastTime))
      -- the clock has wrapped, then the time has passed:
   || ((scheduledTime <= curTime) && (curTime < lastTime))

getTodInTz :: Directory -> String -> IO TimeOfDay
getTodInTz directory tz = liftM localTimeOfDay $ getLocalTimeInTz directory tz

