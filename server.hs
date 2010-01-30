
{-# LANGUAGE PatternGuards #-}

module Main where

import BuildStep
import Config
import ServerMonad
import Utils

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.List
import Data.Time.LocalTime
import Network.Socket
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

baseDir :: FilePath
baseDir = "data"

main :: IO ()
main = do args <- getArgs
          case args of
              []              -> withSocketsDo $ runServer Normal
              ["-v"]          -> withSocketsDo $ runServer Verbose
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
                  writeToFile (clientDir </> "last_build_step_uploaded")
                              (0 :: BuildNum, 0 :: BuildStepNum)
                  writeToFile (clientDir </> "last_build_num_allocated")
                              (0 :: BuildNum)
                  putStrLn "OK, client added"

runServer :: Verbosity -> IO ()
runServer v =
    do addrinfos <- getAddrInfo Nothing Nothing (Just "3000")
       let serveraddr = head addrinfos
       bracket (socket (addrFamily serveraddr) Stream defaultProtocol)
               sClose
               (listenForClients v serveraddr)

listenForClients :: Verbosity -> AddrInfo -> Socket -> IO ()
listenForClients v serveraddr sock
 = do bindSocket sock (addrAddress serveraddr)
      listen sock 1
      let mainLoop = do (conn, _) <- accept sock
                        h <- socketToHandle conn ReadWriteMode
                        hSetBuffering h LineBuffering
                        forkIO $ authClient v h
                        mainLoop
      mainLoop

authClient :: Verbosity -> Handle -> IO ()
authClient v h
 = do msg <- hGetLine h
      when (v >= Verbose) $ putStrLn ("Received: " ++ show msg)
      case stripPrefix "AUTH " msg of
          Just xs ->
              case break (' ' ==) xs of
                  (user, ' ' : pass) ->
                      case lookup user clients of
                      Just ui
                       | ui_password ui == pass ->
                          do tod <- getTOD
                             hPutStrLn h "200 authenticated"
                             let serverState = mkServerState
                                                   h user v tod
                                                   (ui_scheduleTime ui)
                             evalServerMonad handleClient serverState
                      _ ->
                          do hPutStrLn h "501 auth failed"
                             authClient v h
                  _ ->
                      do hPutStrLn h "500 I don't understand"
                         authClient v h
          Nothing ->
              case msg of
                  "HELP" ->
                      -- XXX
                      do hPutStrLn h "500 I don't understand"
                         authClient v h
                  _ ->
                      do hPutStrLn h "500 I don't understand"
                         authClient v h

sendClient :: String -> ServerMonad ()
sendClient str
 = do v <- getVerbosity
      h <- getHandle
      liftIO $ when (v >= Verbose) $ putStrLn ("Sending: " ++ show str)
      liftIO $ hPutStrLn h str

handleClient :: ServerMonad ()
handleClient = do talk
                  handleClient
    where talk :: ServerMonad ()
          talk = do h <- getHandle
                    v <- getVerbosity
                    msg <- liftIO $ hGetLine h
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
                               sendSizedThing h $ mkBuildInstructions thisBuildNum
                        "READY" ->
                            -- XXX Should check times
                            sendClient "200 Nothing to do"
                        _
                         | Just xs <- stripPrefix "UPLOAD " msg,
                           (ys, ' ' : zs) <- break (' ' ==) xs,
                           Just buildNum <- maybeRead ys,
                           Just buildStepNum <- maybeRead zs ->
                            receiveBuildStep buildNum buildStepNum
                         | otherwise ->
                            liftIO $ hPutStrLn h "500 I don't understand"
                    talk

receiveBuildStep :: BuildNum -> BuildStepNum -> ServerMonad ()
receiveBuildStep buildNum buildStepNum
 = do h <- getHandle
      user <- getUser
      let userDir = baseDir </> "clients" </> user
          buildDir = userDir </> show buildNum
          buildStepDir = buildDir </> show buildStepNum
      liftIO $ createDirectoryIfMissing False buildDir
      liftIO $ createDirectoryIfMissing False buildStepDir
      -- Get the program
      sendClient "203 Send program"
      prog <- readSizedThing h
      writeBinaryFile (buildStepDir </> "prog") (show (prog :: String))
      -- Get the args
      sendClient "203 Send args"
      args <- readSizedThing h
      writeBinaryFile (buildStepDir </> "args") (show (args :: [String]))
      -- Get the exit code
      sendClient "203 Send exit code"
      ec <- readSizedThing h
      writeBinaryFile (buildStepDir </> "exitcode") (show (ec :: ExitCode))
      -- Get the stdout
      sendClient "203 Send stdout"
      sOut <- getSizedThing h
      writeBinaryFile (buildStepDir </> "stdout") sOut
      -- Get the stderr
      sendClient "203 Send stderr"
      sErr <- getSizedThing h
      writeBinaryFile (buildStepDir </> "stderr") sErr
      -- update the "last buildnum / buildstep received" counters
      let lastFile = userDir </> "last_build_step_uploaded"
      l <- readFromFile lastFile
      let l' = l `max` (buildNum, buildStepNum)
      writeToFile lastFile l'
      -- and tell the client that we're done, so it can delete its copy
      -- of the files
      sendClient "200 Got it, thanks!"

mkBuildInstructions :: BuildNum -> BuildInstructions
mkBuildInstructions bn = (bn, zip [1..] buildSteps)

scheduledTimePassed :: TimeOfDay -> TimeOfDay -> TimeOfDay -> Bool
scheduledTimePassed lastTime curTime scheduledTime
    = (lastTime < scheduledTime) &&
      (-- the simple case, where the time has passed:
       (curTime >= scheduledTime) ||
       -- the tricky case, where the time has passed,
       -- but then the clock has wrapped:
       (curTime < lastTime))

