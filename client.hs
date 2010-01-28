module Main where

import BuildStep
import Utils

import Network.Socket
import System.IO

remoteHost :: String
remoteHost = "127.0.0.1"

main :: IO ()
main = withSocketsDo $
    do addrinfos <- getAddrInfo Nothing (Just remoteHost) (Just "3000")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       connect sock (addrAddress serveraddr)
       h <- socketToHandle sock ReadWriteMode
       hSetBuffering h LineBuffering

       bs <- getBuildStep h
       runBuildStep bs

       hClose h

getBuildStep :: Handle -> IO BuildStep
getBuildStep h
 = do hPutStrLn h "BUILD INSTRUCTIONS"
      rc <- getResponseCode h
      case rc of
          201 ->
              readSizedThing h
          _ -> die ("Unexpected response code: " ++ show rc)

runBuildStep :: BuildStep -> IO ()
runBuildStep bs = print bs

getResponseCode :: Handle -> IO Int
getResponseCode h = do str <- hGetLine h
                       case maybeRead $ takeWhile (' ' /=) str of
                           Nothing ->
                               die ("Bad response code line: " ++ show str)
                           Just rc ->
                               return rc

