
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import BuildStep
import ClientMonad
import Command
import Files
import Handlelike
import Utils

import Control.Concurrent
import Control.Monad.State
import Network.Socket
import OpenSSL
import OpenSSL.PEM
import OpenSSL.Session
import OpenSSL.X509
import Prelude hiding (catch)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

baseSubDir :: FilePath
baseSubDir = "builder"

getTempBuildDir :: ClientMonad FilePath
getTempBuildDir = do dir <- getBaseDir
                     return (dir </> "tempbuild")

getBuildResultFile :: BuildNum -> ClientMonad FilePath
getBuildResultFile bn = do dir <- getBaseDir
                           return (dir </> "builds" </> show bn </> "result")

main :: IO ()
main = do args <- getArgs
          case args of
              []       -> withSocketsDo $ withOpenSSL $ runClient Normal
              ["-v"]   -> withSocketsDo $ withOpenSSL $ runClient Verbose
              -- XXX user and pass oughtn't really be given on the
              -- commandline, but hey
              ["init", user, pass, host] -> initClient user pass host
              _        -> die "Bad args"

initClient :: String -> String -> String -> IO ()
initClient user pass host
 = do -- XXX We really ought to catch an already-exists
      -- exception and handle it properly
      createDirectory baseSubDir
      createDirectory (baseSubDir </> "builds")
      writeBinaryFile (baseSubDir </> "user") user
      writeBinaryFile (baseSubDir </> "pass") pass
      writeBinaryFile (baseSubDir </> "host") host

runClient :: Verbosity -> IO ()
runClient v = do curDir <- getCurrentDirectory
                 let baseDir = curDir </> baseSubDir
                 host <- readBinaryFile (baseDir </> "host")
                 connLoop baseDir host
    where connLoop baseDir host
              = do s <- conn host 5
                   verbose' v "Connected."

                   let client = mkClientState v host baseDir (Socket s)
                   evalClientMonad doClient client
              `onEndOfFile` connLoop baseDir host

          conn host secs
              = do verbose' v "Connecting..."
                   c host `onDoesNotExist`
                          do verbose' v ("Failed...sleeping for " ++ show secs ++ " seconds...")
                             threadDelay (secs * 1000000)
                             conn host ((secs * 2) `min` 600)

          c host = do addrinfos <- getAddrInfo Nothing (Just host)
                                                       (Just (show port))
                      let serveraddr = head addrinfos
                      sock <- socket (addrFamily serveraddr) Stream
                                     defaultProtocol
                      Network.Socket.connect sock (addrAddress serveraddr)
                      return sock

doClient :: ClientMonad ()
doClient = do startSsl
              authenticate
              uploadAllBuildResults
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

verbose :: String -> ClientMonad ()
verbose str = do v <- getVerbosity
                 liftIO $ verbose' v str

verbose' :: Verbosity -> String -> IO ()
verbose' v str = when (v >= Verbose) $ putStrLn str

sendServer :: String -> ClientMonad ()
sendServer str = do verbose ("Sending: " ++ show str)
                    hlPutStrLn str

wantSsl :: Bool
wantSsl = True

startSsl :: ClientMonad ()
startSsl
 | wantSsl   = do h <- getHandle
                  case h of
                      Socket sock -> do
                          sendServer "START SSL"
                          clientPem <- liftIO $ readFile "client.pem"
                          clientX509 <- liftIO $ readX509 clientPem
                          clientPrivateKey <- liftIO $ readPrivateKey clientPem (PwStr "password")
                          sslContext <- liftIO $ context
                          liftIO $ contextSetCertificate sslContext clientX509
                          liftIO $ contextSetPrivateKey sslContext clientPrivateKey
                          liftIO $ contextSetCAFile sslContext "root.pem"
                          ssl <- liftIO $ OpenSSL.Session.connection sslContext sock
                          liftIO $ OpenSSL.Session.connect ssl
                          verifySsl ssl
                          setHandle (Ssl ssl)
                          getTheResponseCode 200
                      _ ->
                          error "XXX Can't happen: Expected a socket"
 | otherwise = do sendServer "NO SSL"
                  getTheResponseCode 200

verifySsl :: SSL -> ClientMonad ()
verifySsl ssl
 = do verified <- liftIO $ getVerifyResult ssl
      unless verified $ die "Certificate doesn't verify"
      mPeerCert <- liftIO $ getPeerCertificate ssl
      case mPeerCert of
          Nothing ->
              die "No peer certificate"
          Just peerCert ->
              do mapping <- liftIO $ getSubjectName peerCert False
                 case lookup "CN" mapping of
                     Just host' ->
                         do host <- getHost
                            unless (host == host') $ die "Certificate is for the wrong host"
                     Nothing ->
                         die "Certificate has no host"

authenticate :: ClientMonad ()
authenticate = do dir <- getBaseDir
                  user <- readBinaryFile (dir </> "user")
                  pass <- readBinaryFile (dir </> "pass")
                  sendServer ("AUTH " ++ user ++ " " ++ pass)
                  getTheResponseCode 200

getBuildInstructions :: ClientMonad BuildInstructions
getBuildInstructions
 = do sendServer "BUILD INSTRUCTIONS"
      getTheResponseCode 201
      bi <- readSizedThing
      getTheResponseCode 200
      return bi

runBuildInstructions :: BuildInstructions -> ClientMonad ()
runBuildInstructions (bn, bss)
 = do baseDir <- getBaseDir
      let root = Client baseDir
          buildDir = baseDir </> "builds" </> show bn
      liftIO $ createDirectory buildDir
      writeBuildResult root bn Incomplete
      liftIO $ createDirectory (buildDir </> "steps")
      tempBuildDir <- getTempBuildDir
      liftIO $ ignoreDoesNotExist $ removeDirectoryRecursive tempBuildDir
      liftIO $ createDirectory tempBuildDir
      runBuildSteps bn bss

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
      let root   = Client baseDir
          name   = bs_name   bs
          subdir = bs_subdir bs
          prog   = bs_prog   bs
          args   = bs_args   bs
          buildStepDir = baseDir </> "builds" </> show bn </> "steps" </> show bsn
      liftIO $ createDirectory buildStepDir
      writeBuildStepName     root bn bsn name
      writeBuildStepSubdir   root bn bsn subdir
      writeBuildStepProg     root bn bsn prog
      writeBuildStepArgs     root bn bsn args
      ec <- liftIO $ run prog args (buildStepDir </> "output")
      writeBuildStepExitcode root bn bsn ec
      return ec

uploadAllBuildResults :: ClientMonad ()
uploadAllBuildResults
 = do baseDir <- getBaseDir
      let buildsDir = baseDir </> "builds"
      bns <- liftIO $ getSortedNumericDirectoryContents buildsDir
      unless (null bns) $
          do sendServer "LAST UPLOADED"
             getTheResponseCode 201
             num <- readSizedThing
             getTheResponseCode 200
             case span (<= num) bns of
                 (ys, zs) ->
                     do mapM_ removeBuildNum ys
                        mapM_ uploadBuildResults zs

removeBuildNum :: BuildNum -> ClientMonad ()
removeBuildNum bn
 = do baseDir <- getBaseDir
      let buildDir = baseDir </> "builds" </> show bn
      liftIO $ removeDirectoryRecursive buildDir

uploadBuildResults :: BuildNum -> ClientMonad ()
uploadBuildResults bn
 = do baseDir <- getBaseDir
      let root = Client baseDir
          buildDir = baseDir </> "builds" </> show bn
          stepsDir = buildDir </> "steps"
          sendString f = do getTheResponseCode 203
                            m <- f
                            putMaybeSizedThing m
          sendStep bsn
              = do let stepDir = stepsDir </> show bsn
                       strings = [getMaybeBuildStepName     root bn bsn,
                                  getMaybeBuildStepSubdir   root bn bsn,
                                  getMaybeBuildStepProg     root bn bsn,
                                  getMaybeBuildStepArgs     root bn bsn,
                                  getMaybeBuildStepExitcode root bn bsn,
                                  getMaybeBuildStepOutput   root bn bsn]
                   sendServer ("UPLOAD " ++ show bn ++ " " ++ show bsn)
                   mapM_ sendString strings
                   getTheResponseCode 200
                   removeBuildStepName     root bn bsn
                   removeBuildStepSubdir   root bn bsn
                   removeBuildStepProg     root bn bsn
                   removeBuildStepArgs     root bn bsn
                   removeBuildStepExitcode root bn bsn
                   removeBuildStepOutput   root bn bsn
                   liftIO $ removeDirectory stepDir
      bsns <- liftIO $ getSortedNumericDirectoryContents stepsDir
      mapM_ sendStep bsns
      liftIO $ removeDirectory stepsDir
      sendServer ("RESULT " ++ show bn)
      sendString $ getMaybeBuildResult root bn
      getTheResponseCode 200
      removeBuildResult root bn
      liftIO $ removeDirectory buildDir

getTheResponseCode :: Int -> ClientMonad ()
getTheResponseCode n = getAResponseCode [n]

getAResponseCode :: [Int] -> ClientMonad ()
getAResponseCode ns = do rc <- getResponseCode
                         unless (rc `elem` ns) $ die "Bad response code"

getResponseCode :: ClientMonad Int
getResponseCode = do v <- getVerbosity
                     str <- hlGetLine
                     liftIO $ when (v >= Verbose) $
                         putStrLn ("Received: " ++ show str)
                     case maybeRead $ takeWhile (' ' /=) str of
                         Nothing ->
                             die ("Bad response code line: " ++ show str)
                         Just rc ->
                             return rc

