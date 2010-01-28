
module Main where

import BuildStep
import Utils

import Control.Concurrent
import Network.Socket
import System.IO

main :: IO ()
main = withSocketsDo $
    do addrinfos <- getAddrInfo Nothing Nothing (Just "3000")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       bindSocket sock (addrAddress serveraddr)
       listen sock 1
       let mainLoop = do (conn, _) <- accept sock
                         h <- socketToHandle conn ReadWriteMode
                         forkIO $ handleClient h
                         hSetBuffering h LineBuffering
                         mainLoop
       mainLoop
       sClose sock

handleClient :: Handle -> IO ()
handleClient h = do talk
                    hClose h
    where talk :: IO ()
          talk = do msg <- hGetLine h
                    case msg of
                        "BUILD INSTRUCTIONS" ->
                            do hPutStrLn h "201 Instructions follow"
                               sendSizedThing h buildStep
                        _ ->
                            hPutStrLn h "500 I don't understand"
                    talk

buildStep :: BuildStep
buildStep = BuildStep {
                bs_name = "Test build step",
                bs_prog = "/bin/echo",
                bs_args = ["arg1", "arg2", "arg3"]
            }

