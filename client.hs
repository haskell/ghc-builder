
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import BuildStep
import Command
import Utils

import Control.Concurrent
import Control.Monad.State
import Data.List
import Network.Socket
import System.Directory
import System.Environment
import System.FilePath
import System.IO

remoteHost :: String
remoteHost = "127.0.0.1"

baseSubDir :: FilePath
baseSubDir = "builds"

main :: IO ()
main = do args <- getArgs
          case args of
              []       -> withSocketsDo runClient
              ["init"] -> initClient
              _        -> die "Bad args"

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

initClient :: IO ()
initClient = do -- XXX We really ought to catch an already-exists
                -- exception and handle it properly
                createDirectory baseSubDir

runClient :: IO ()
runClient =
    do addrinfos <- getAddrInfo Nothing (Just remoteHost) (Just "3000")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       connect sock (addrAddress serveraddr)
       h <- socketToHandle sock ReadWriteMode
       hSetBuffering h LineBuffering

       curDir <- getCurrentDirectory
       let client = ClientState {
                        cs_basedir = curDir </> baseSubDir,
                        cs_handle = h
                    }
       evalClientMonad doClient client

doClient :: ClientMonad ()
doClient
 = do authenticate
      bi <- getBuildInstructions
      runBuildInstructions bi
      uploadBuildResults (fst bi)

mainLoop :: ClientMonad ()
mainLoop
 = do sendServer "READY"
      rc <- getResponseCode
      case rc of
          200 -> liftIO $ threadDelay (5 * 60 * 1000000)
          202 -> do bi <- getBuildInstructions
                    runBuildInstructions bi
                    -- XXX We will arrange it such that everything is
                    -- uploaded by the time we get here, so the build
                    -- we've just done is the next one to be uploaded
                    uploadBuildResults (fst bi)
          _ -> die ("Unexpected response code: " ++ show rc)
      mainLoop

sendServer :: String -> ClientMonad ()
sendServer str = do liftIO $ when True $ -- XXX
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
      mapM_ (runBuildStep bn) bss

runBuildStep :: BuildNum -> (BuildStepNum, BuildStep) -> ClientMonad ()
runBuildStep bn (bsn, bs)
 = do liftIO $ putStrLn ("Running " ++ show (bs_name bs))
      baseDir <- getBaseDir
      let prog = bs_prog bs
          args = bs_args bs
          buildStepDir = baseDir </> show bn </> show bsn
      (sOut, sErr, ec) <- liftIO $ run prog args
      liftIO $ createDirectory buildStepDir
      liftIO $ writeBinaryFile (buildStepDir </> "prog") (show prog)
      liftIO $ writeBinaryFile (buildStepDir </> "args") (show args)
      liftIO $ writeBinaryFile (buildStepDir </> "stdout") sOut
      liftIO $ writeBinaryFile (buildStepDir </> "stderr") sErr
      liftIO $ writeBinaryFile (buildStepDir </> "exitcode") (show ec)

uploadBuildResults :: BuildNum -> ClientMonad ()
uploadBuildResults bn
 = do baseDir <- getBaseDir
      let buildDir = baseDir </> show bn
          sendStep bsn
              = do h <- getHandle
                   let stepDir = buildDir </> show bsn
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
      liftIO $ removeDirectory buildDir

getTheResponseCode :: Int -> ClientMonad ()
getTheResponseCode n = do rc <- getResponseCode
                          unless (rc == n) $ die "Bad response code"

getResponseCode :: ClientMonad Int
getResponseCode = do h <- getHandle
                     str <- liftIO $ hGetLine h
                     case maybeRead $ takeWhile (' ' /=) str of
                         Nothing ->
                             die ("Bad response code line: " ++ show str)
                         Just rc ->
                             return rc

