
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
import Data.Time.Format
import Data.Time.LocalTime
import Network.Socket
import OpenSSL
import OpenSSL.PEM
import OpenSSL.Session
import OpenSSL.X509
import Prelude hiding (catch)
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Locale
import System.Posix.Signals

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          hSetBuffering stderr LineBuffering
          args <- getArgs
          case args of
              []              -> runServer Nothing  Normal
              ["-v"]          -> runServer Nothing  Verbose
              ["-o", o]       -> runServer (Just o) Normal
              ["-v", "-o", o] -> runServer (Just o) Verbose
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

runServer :: Maybe FilePath -> Verbosity -> IO ()
runServer mfp v = withSocketsDo $ withOpenSSL $
    do messagerVar <- newEmptyMVar
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
       persistentThread "Messager"       (messager directory mfph v)
       persistentThread "Notification"   (notifier directory)
       persistentThread "Config handler" (configHandler configHandlerVar)
       persistentThread "Time"           (timeMaster timeMasterVar)
       _ <- installHandler sigHUP
                           (Catch (gotSigHUP directory))
                           Nothing
       addrinfos <- getAddrInfo Nothing (Just "0.0.0.0") (Just (show port))
       let serveraddr = head addrinfos
       bracket (socket (addrFamily serveraddr) Stream defaultProtocol)
               sClose
               (listenForClients directory serveraddr)

gotSigHUP :: Directory -> IO ()
gotSigHUP directory
 = do verbose' directory Main "Reloading config"
      putMVar (dir_messagerVar directory) Reopen
      putMVar (dir_configHandlerVar directory) ReloadConfig

persistentThread :: String -> IO () -> IO ()
persistentThread title f
 = do let thread = f `catch` \e ->
                       do warn (exceptionMsg e)
                          thread
      _ <- forkIO thread
      return ()
    where exceptionMsg e = unlines [title ++ " thread got an exception:",
                                    show (e :: SomeException),
                                    "Restarting..."]

listenForClients :: Directory -> AddrInfo -> Socket -> IO ()
listenForClients directory serveraddr sock
 = do bindSocket sock (addrAddress serveraddr)
      listen sock 1
      let defaultProtocolVersion = 0.1
          mainLoop = do (conn, _) <- Network.Socket.accept sock
                        _ <- forkIO $ startSsl defaultProtocolVersion directory conn
                        mainLoop
      mainLoop

fpServerPem :: FilePath
fpServerPem = "certs/server.pem"

fpRootPem :: FilePath
fpRootPem = "certs/root.pem"

startSsl :: ProtocolVersion -> Directory -> Socket -> IO ()
startSsl pv directory s
 = do msg <- hlGetLine' s
      verbose' directory Unauthed ("Received: " ++ show msg)
      case stripPrefix "PROTO " msg of
          Just verStr ->
              case maybeRead verStr of
                  Just pv'
                   | pv' `elem` [0.1, 0.2] ->
                      do sendHandle directory s respOK "Protocol version OK"
                         startSsl pv' directory s
                  _ ->
                      do sendHandle directory s respHuh "Protocol version not recognised"
                         startSsl pv directory s
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
                         mUser <- verifySsl ssl
                         sendHandle directory ssl respOK "Welcome to SSL"
                         authClient pv directory (Ssl ssl) mUser
                  "NO SSL" ->
                      do sendHandle directory s respOK "OK, no SSL"
                         authClient pv directory (Socket s) Nothing
                  _ -> do sendHandle directory s respHuh "Expected protocol version or SSL instructions"
                          startSsl pv directory s

verifySsl :: SSL -> IO (Maybe User)
verifySsl ssl
 = do verified <- getVerifyResult ssl
      unless verified $ die "Certificate doesn't verify"
      mPeerCert <- getPeerCertificate ssl
      case mPeerCert of
          Nothing ->
              -- XXX We're hitting this branch. Is that expected?
              -- die "No peer certificate"
              return Nothing
          Just peerCert ->
              do mapping <- getSubjectName peerCert False
                 case lookup "CN" mapping of
                     Just user ->
                         return (Just user)
                     Nothing ->
                         die "Certificate has no CN"

authClient :: ProtocolVersion -> Directory -> HandleOrSsl -> Maybe User
           -> IO ()
authClient pv directory h mu
 = do msg <- hlGetLine' h
      verbose' directory Unauthed ("Received: " ++ show msg)
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
                             sendHandle directory h respOK "authenticated"
                             let serverState = mkServerState
                                                   h user pv directory tod
                             evalServerMonad handleClient serverState
                                 `finally`
                                 verbose' directory (User user) "Disconnected"
                  _ ->
                      do sendHandle directory h respHuh "I don't understand"
                         authClient pv directory h mu
          Nothing ->
              case msg of
                  "HELP" ->
                      -- XXX
                      do sendHandle directory h respHuh "I don't understand"
                         authClient pv directory h mu
                  _ ->
                      do sendHandle directory h respHuh "I don't understand"
                         authClient pv directory h mu
    where authFailed reason = do verbose' directory Unauthed ("Auth failed: " ++ reason)
                                 sendHandle directory h respAuthFailed "auth failed"
                                 authClient pv directory h mu

verbose :: String -> ServerMonad ()
verbose str = do directory <- getDirectory
                 u <- getUser
                 liftIO $ verbose' directory (User u) str

verbose' :: Directory -> Who -> String -> IO ()
verbose' directory w str
 = do lt <- getLocalTimeInTz directory "UTC"
      let fmt = "[%Y-%m-%d %H:%M:%S]"
          t = formatTime defaultTimeLocale fmt lt
      putMVar (dir_messagerVar directory)
              (Message Verbose $ unwords [t, pprWho w, str])

data Who = User User | Unauthed | Notifier | Main

pprWho :: Who -> String
pprWho (User u) = "[U:" ++ u ++ "]"
pprWho Unauthed = "[unauthed]"
pprWho Notifier = "[notifier]"
pprWho Main     = "[main]"

sendHandle :: Handlelike h => Directory -> h -> Response -> String -> IO ()
sendHandle directory h resp str
 = do let respStr = show resp ++ " " ++ str
      verbose' directory Unauthed ("Sending: " ++ show respStr)
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
      -- Get the mailOutput
      unless (pv == 0.1) $ do
          sendClient respSendSizedThing "Send mailOutput"
          mMailOutput <- getMaybeSizedThing
          putMaybeBuildStepMailOutput root buildNum buildStepNum mMailOutput
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

getLocalTimeInTz :: Directory -> String -> IO LocalTime
getLocalTimeInTz directory tz
 = do mv <- newEmptyMVar
      putMVar (dir_timeMasterVar directory) (tz, mv)
      takeMVar mv

getTodInTz :: Directory -> String -> IO TimeOfDay
getTodInTz directory tz = liftM localTimeOfDay $ getLocalTimeInTz directory tz

