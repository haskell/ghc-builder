
module Builder.Command where

import Builder.Utils

import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Exit
import System.IO
import System.Process

data Line = Stdout String
          | Stderr String
    deriving (Show, Read)

run :: FilePath -> [String] -> FilePath -> Maybe FilePath -> IO ExitCode
run prog args combinedOutputFile mStdoutFile
 = do (hIn, hOut, hErr, ph) <- runInteractiveProcess prog args Nothing Nothing
      hClose hIn
      hCombinedOutput <- openFile combinedOutputFile WriteMode
      mv <- newEmptyMVar
      let getLines h m c = do l <- hGetLine h
                              case m of
                                  Just outh -> hPutStr outh l
                                  Nothing -> return ()
                              putMVar mv (Just (c l))
                              getLines h m c
          writeLines :: Int -- how many of stdout and stderr are till open
                     -> IO ()
          writeLines 0 = hClose hCombinedOutput
          writeLines n = do mLine <- takeMVar mv
                            case mLine of
                                Just line ->
                                    do hPutStrLn hCombinedOutput (show line)
                                       writeLines n
                                Nothing -> writeLines (n - 1)
      _ <- forkIO $ (do hSetBuffering hOut LineBuffering
                        bracket (case mStdoutFile of
                                 Nothing -> return Nothing
                                 Just f -> liftM Just $ openFile f WriteMode)
                                (\mh -> case mh of
                                        Just h -> hClose h
                                        Nothing -> return ())
                                (\mh -> ignoreEndOfFile $
                                        getLines hOut mh Stdout))
                     `finally`
                     putMVar mv Nothing
      _ <- forkIO $ (do hSetBuffering hErr LineBuffering
                        ignoreEndOfFile $ getLines hErr Nothing Stderr)
                     `finally`
                     putMVar mv Nothing
      _ <- forkIO $ writeLines 2

      waitForProcess ph
 `onDoesNotExist`
    return (ExitFailure 1)

