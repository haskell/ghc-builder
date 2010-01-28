module Main where

import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv)
import Network.Socket.ByteString

remoteHost :: String
remoteHost = "127.0.0.1"

main :: IO ()
main = withSocketsDo $
    do addrinfos <- getAddrInfo Nothing (Just remoteHost) (Just "3000")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       connect sock (addrAddress serveraddr)
       sendAll sock $ C.pack "Hello, world!"
       msg <- recv sock 1024
       sClose sock
       putStr "Received "
       C.putStrLn msg
