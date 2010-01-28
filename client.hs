module Main where

import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv)
import Network.Socket.ByteString
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

       let sendMsg = do hPutStrLn h "Hello, world!"
                        msg <- hGetLine h
                        putStrLn ("Received " ++ msg)

       sendMsg
       sendMsg
       sendMsg
       sendMsg
       sendMsg

       hClose h

