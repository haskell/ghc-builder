module Main where

import BuildStep
import Command
import Utils

import Network.Socket
import System.Directory
import System.FilePath
import System.IO

remoteHost :: String
remoteHost = "127.0.0.1"

baseDir :: FilePath
baseDir = "builds"

main :: IO ()
main = withSocketsDo $
    do addrinfos <- getAddrInfo Nothing (Just remoteHost) (Just "3000")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       connect sock (addrAddress serveraddr)
       h <- socketToHandle sock ReadWriteMode
       hSetBuffering h LineBuffering

       bi <- getBuildInstructions h
       runBuildInstructions bi

       hClose h

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
      writeBinaryFile (baseDir </> show bn </> show bsn <.> "prog") (show prog)
      writeBinaryFile (baseDir </> show bn </> show bsn <.> "args") (show args)
      writeBinaryFile (baseDir </> show bn </> show bsn <.> "stdout") sOut
      writeBinaryFile (baseDir </> show bn </> show bsn <.> "stderr") sErr
      writeBinaryFile (baseDir </> show bn </> show bsn <.> "ec") (show ec)

getResponseCode :: Handle -> IO Int
getResponseCode h = do str <- hGetLine h
                       case maybeRead $ takeWhile (' ' /=) str of
                           Nothing ->
                               die ("Bad response code line: " ++ show str)
                           Just rc ->
                               return rc

