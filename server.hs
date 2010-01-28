
module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as S
import Network.Socket hiding (recv)
import Network.Socket.ByteString
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
          talk = do putStrLn "Getting"
                    msg <- hGetLine h
                    unless (null msg) $
                        do hPutStrLn h msg
                           talk


