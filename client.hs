
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import BuildStep
import ClientMonad
import Command
import Utils

import Control.Concurrent
import Control.Monad.State
import Data.List
import Network.Socket
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

remoteHost :: String
remoteHost = "127.0.0.1"

baseSubDir :: FilePath
baseSubDir = "builds"

getTempBuildDir :: ClientMonad FilePath
getTempBuildDir = do dir <- getBaseDir
                     return (dir </> "tempbuild")

getBuildResultFile :: BuildNum -> ClientMonad FilePath
getBuildResultFile bn = do dir <- getBaseDir
                           return (dir </> show bn </> "result")

main :: IO ()
main = do args <- getArgs
          case args of
              []       -> withSocketsDo $ runClient Normal
              ["-v"]   -> withSocketsDo $ runClient Verbose
              ["init"] -> initClient
              _        -> die "Bad args"

initClient :: IO ()
initClient = do -- XXX We really ought to catch an already-exists
                -- exception and handle it properly
                createDirectory baseSubDir

runClient :: Verbosity -> IO ()
runClient v =
    do addrinfos <- getAddrInfo Nothing (Just remoteHost) (Just "3000")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       connect sock (addrAddress serveraddr)
       h <- socketToHandle sock ReadWriteMode
       hSetBuffering h LineBuffering

       curDir <- getCurrentDirectory
       let client = mkClientState v (curDir </> baseSubDir) h
       evalClientMonad doClient client

doClient :: ClientMonad ()
doClient = do authenticate
              mainLoop

mainLoop :: ClientMonad ()
mainLoop
 = do sendServer "READY"
      rc <- getResponseCode
      case rc of
          200 -> liftIO $ threadDelay (5 * 60 * 1000000)
          202 -> doABuild
          _ -> die ("Unexpected response code: " ++ show rc)
      mainLoop

doABuild :: ClientMonad ()
doABuild = do bi <- getBuildInstructions
              runBuildInstructions bi
              -- XXX We will arrange it such that everything is
              -- uploaded by the time we get here, so the build
              -- we've just done is the next one to be uploaded
              uploadBuildResults (fst bi)
              -- We've just been doing stuff for a long time
              -- potentially, so we could have overrun a scheduled
              -- build by a long time. In that case, we don't want
              -- to start a build hours late, so we tell the server
              -- READY to reset the "last READY time", but we don't
              -- start a build if it tells us we should
              sendServer "READY"
              getAResponseCode [200, 202]

sendServer :: String -> ClientMonad ()
sendServer str = do v <- getVerbosity
                    liftIO $ when (v >= Verbose) $
                        putStrLn ("Sending: " ++ show str)
                    h <- getHandle
                    liftIO $ hPutStrLn h str

authenticate :: ClientMonad ()
authenticate = do sendServer "AUTH foo mypass"
                  getTheResponseCode 200

getBuildInstructions :: ClientMonad BuildInstructions
getBuildInstructions
 = do sendServer "BUILD INSTRUCTIONS"
      getTheResponseCode 201
      h <- getHandle
      liftIO $ readSizedThing h

runBuildInstructions :: BuildInstructions -> ClientMonad ()
runBuildInstructions (bn, bss)
 = do baseDir <- getBaseDir
      liftIO $ createDirectory (baseDir </> show bn)
      liftIO $ createDirectory (baseDir </> show bn </> "steps")
      tempBuildDir <- getTempBuildDir
      liftIO $ ignoreDoesNotExist $ removeDirectoryRecursive tempBuildDir
      liftIO $ createDirectory tempBuildDir
      mapM_ (runBuildStep bn) bss

runBuildSteps :: BuildNum -> [(BuildStepNum, BuildStep)] -> ClientMonad ()
runBuildSteps bn [] = do fp <- getBuildResultFile bn
                         writeToFile fp Success
runBuildSteps bn (step : steps)
 = do ec <- runBuildStep bn step
      case ec of
          ExitSuccess ->
              runBuildSteps bn steps
          _ ->
              do fp <- getBuildResultFile bn
                 writeToFile fp Failure

runBuildStep :: BuildNum -> (BuildStepNum, BuildStep) -> ClientMonad ExitCode
runBuildStep bn (bsn, bs)
 = do liftIO $ putStrLn ("Running " ++ show (bs_name bs))
      baseDir <- getBaseDir
      tempBuildDir <- getTempBuildDir
      liftIO $ setCurrentDirectory (tempBuildDir </> bs_subdir bs)
      let prog = bs_prog bs
          args = bs_args bs
          buildStepDir = baseDir </> show bn </> "steps" </> show bsn
      (sOut, sErr, ec) <- liftIO $ run prog args
      liftIO $ createDirectory buildStepDir
      liftIO $ writeBinaryFile (buildStepDir </> "prog") (show prog)
      liftIO $ writeBinaryFile (buildStepDir </> "args") (show args)
      liftIO $ writeBinaryFile (buildStepDir </> "stdout") sOut
      liftIO $ writeBinaryFile (buildStepDir </> "stderr") sErr
      liftIO $ writeBinaryFile (buildStepDir </> "exitcode") (show ec)
      return ec

uploadBuildResults :: BuildNum -> ClientMonad ()
uploadBuildResults bn
 = do baseDir <- getBaseDir
      h <- getHandle
      let buildDir = baseDir </> show bn
          sendStep bsn
              = do let stepDir = buildDir </> show bsn
                       sendFile f = do getTheResponseCode 203
                                       xs <- liftIO $ readBinaryFile f
                                       liftIO $ putSizedThing h xs
                       files = map (stepDir </>)
                                   ["prog", "args", "exitcode",
                                    "stdout", "stderr"]
                   sendServer ("UPLOAD " ++ show bn ++ " " ++ show bsn)
                   mapM_ sendFile files
                   getTheResponseCode 200
                   liftIO $ mapM_ removeFile files
                   liftIO $ removeDirectory stepDir
      bsns <- liftIO $ getNumericDirectoryContents buildDir
      mapM_ sendStep $ sort bsns
      sendServer ("RESULT " ++ show bn)
      xs <- liftIO $ readBinaryFile (buildDir </> "result")
      liftIO $ putSizedThing h xs
      liftIO $ removeDirectory buildDir

getTheResponseCode :: Int -> ClientMonad ()
getTheResponseCode n = getAResponseCode [n]

getAResponseCode :: [Int] -> ClientMonad ()
getAResponseCode ns = do rc <- getResponseCode
                         unless (rc `elem` ns) $ die "Bad response code"

getResponseCode :: ClientMonad Int
getResponseCode = do h <- getHandle
                     str <- liftIO $ hGetLine h
                     case maybeRead $ takeWhile (' ' /=) str of
                         Nothing ->
                             die ("Bad response code line: " ++ show str)
                         Just rc ->
                             return rc

