
module Messager where

import ServerMonad
import Builder.Utils

import Control.Concurrent.MVar
import Control.Monad
import System.IO

messager :: Directory -> Maybe (FilePath, Maybe Handle) -> Verbosity -> IO ()
messager directory (Just (fp, Nothing)) v
 = do h <- openFile fp AppendMode
      messager directory (Just (fp, Just h)) v
messager directory m v
 = do req <- takeMVar (dir_messagerVar directory)
      case req of
          Message v' str ->
              do case m of
                     Just (_, Just h) ->
                         hPutStrLn h str
                     _ ->
                         return ()
                 when (v >= v') $ putStrLn str
                 messager directory m v
          Reopen ->
              do case m of
                     Just (fp, Just h) ->
                         do hClose h
                            messager directory (Just (fp, Nothing)) v
                     _ ->
                         messager directory m v

