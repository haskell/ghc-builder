module Main where

import BuildStep
import Command
import Utils

import Control.Concurrent
import Control.Monad
import Data.List
import Network.Socket
import System.Directory
import System.Environment
import System.FilePath
import System.IO

remoteHost :: String
remoteHost = "127.0.0.1"

baseDir :: FilePath
baseDir = "builds"

main :: IO ()
main = do args <- getArgs
          case args of
              []       -> withSocketsDo runClient
              ["init"] -> initClient
              _        -> die "Bad args"

initClient :: IO ()
initClient = do -- XXX We really ought to catch an already-exists
                -- exception and handle it properly
                createDirectory baseDir

runClient :: IO ()
runClient =
    do addrinfos <- getAddrInfo Nothing (Just remoteHost) (Just "3000")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       connect sock (addrAddress serveraddr)
       h <- socketToHandle sock ReadWriteMode
       hSetBuffering h LineBuffering

       authenticate h
       bi <- getBuildInstructions h
       runBuildInstructions bi

       -- XXX Kind of pointless, and won't work well once we do reconnection:
       hClose h

mainLoop :: Handle -> IO ()
mainLoop h
 = do hPutStrLn h "READY"
      rc <- getResponseCode h
      case rc of
          200 -> threadDelay (5 * 60 * 1000000)
          202 -> do bi <- getBuildInstructions h
                    runBuildInstructions bi
                    -- XXX We will arrange it such that everything is
                    -- uploaded by the time we get here, so the build
                    -- we've just done is the next one to be uploaded
                    uploadBuildResults h (fst bi)
          _ -> die ("Unexpected response code: " ++ show rc)
      mainLoop h

authenticate :: Handle -> IO ()
authenticate h = do hPutStrLn h "AUTH foo mypass"
                    getTheResponseCode 200 h

getBuildInstructions :: Handle -> IO BuildInstructions
getBuildInstructions h
 = do hPutStrLn h "BUILD INSTRUCTIONS"
      rc <- getResponseCode h
      case rc of
          201 ->
              readSizedThing h
          _ -> die ("Unexpected response code: " ++ show rc)

runBuildInstructions :: BuildInstructions -> IO ()
runBuildInstructions (bn, bss) = do createDirectory (baseDir </> show bn)
                                    mapM_ (runBuildStep bn) bss

runBuildStep :: BuildNum -> (BuildStepNum, BuildStep) -> IO ()
runBuildStep bn (bsn, bs)
 = do putStrLn ("Running " ++ show (bs_name bs))
      let prog = bs_prog bs
          args = bs_args bs
      (sOut, sErr, ec) <- run prog args
      writeBinaryFile (baseDir </> show bn </> show bsn </> "prog") (show prog)
      writeBinaryFile (baseDir </> show bn </> show bsn </> "args") (show args)
      writeBinaryFile (baseDir </> show bn </> show bsn </> "stdout") sOut
      writeBinaryFile (baseDir </> show bn </> show bsn </> "stderr") sErr
      writeBinaryFile (baseDir </> show bn </> show bsn </> "ec") (show ec)

uploadBuildResults :: Handle -> BuildNum -> IO ()
uploadBuildResults h bn
 = do bsns <- getInterestingDirectoryContents buildDir
      mapM_ sendStep $ sort bsns
      removeDirectory buildDir
    where buildDir = baseDir </> show bn
          sendStep bsn
              = do let stepDir = buildDir </> show bsn
                       sendFile f = do getTheResponseCode 203 h
                                       xs <- readBinaryFile f
                                       putSizedThing h xs
                       files = map (stepDir </>)
                                   ["prog", "args", "exitcode",
                                    "stdout", "stderr"]
                   hPutStrLn h ("UPLOAD " ++ show bn ++ " " ++ show bsn)
                   mapM_ sendFile files
                   getTheResponseCode 200 h
                   mapM_ removeFile files
                   removeDirectory stepDir

getTheResponseCode :: Int -> Handle -> IO ()
getTheResponseCode n h = do rc <- getResponseCode h
                            unless (rc == n) $ die "Bad response code"

getResponseCode :: Handle -> IO Int
getResponseCode h = do str <- hGetLine h
                       case maybeRead $ takeWhile (' ' /=) str of
                           Nothing ->
                               die ("Bad response code line: " ++ show str)
                           Just rc ->
                               return rc

