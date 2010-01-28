
module Command where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import System.Exit
import System.IO
import System.Process

run :: FilePath -> [String] -> IO (String, String, ExitCode)
run prog args
 = do (hIn, hOut, hErr, ph) <- runInteractiveProcess prog args Nothing Nothing
      hClose hIn
      mv <- newEmptyMVar
      sOut <- hGetContents hOut
      sErr <- hGetContents hErr
      forkIO $ (do evaluate (length sOut)
                   return ()
                `finally`
                putMVar mv ())
      forkIO $ (do evaluate (length sErr)
                   return ()
                `finally`
                putMVar mv ())
      ec <- waitForProcess ph
      takeMVar mv
      takeMVar mv
      return (sOut, sErr, ec)
