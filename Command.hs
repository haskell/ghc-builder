
module Command where

import Utils

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import System.Exit
import System.IO
import System.Process

data Line = Stdout String
          | Stderr String
    deriving (Show, Read)

run :: FilePath -> [String] -> FilePath -> IO ExitCode
run prog args outputFile
 = do (hIn, hOut, hErr, ph) <- runInteractiveProcess prog args Nothing Nothing
      hClose hIn
      hOutput <- openFile outputFile WriteMode
      mv <- newEmptyMVar
      let getLines h c = do l <- hGetLine h
                            putMVar mv (Just (c l))
                            getLines h c
          writeLines :: Int -- how many of stdout and stderr are till open
                     -> IO ()
          writeLines 0 = hClose hOutput
          writeLines n = do mLine <- takeMVar mv
                            case mLine of
                                Just line -> do hPutStrLn hOutput (show line)
                                                writeLines n
                                Nothing -> writeLines (n - 1)
      forkIO $ (do hSetBuffering hOut LineBuffering
                   getLines hOut Stdout `onEndOfFile` return ())
                `finally`
                putMVar mv Nothing
      forkIO $ (do hSetBuffering hErr LineBuffering
                   getLines hErr Stderr `onEndOfFile` return ())
                `finally`
                putMVar mv Nothing
      forkIO $ writeLines 2

      waitForProcess ph

