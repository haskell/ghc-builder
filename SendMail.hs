
module SendMail (sendMail) where

import Codec.MIME.String.Flatten
import Codec.MIME.String.Parse
import Control.Concurrent
import Control.Concurrent.MVar
import Data.List
import System.Exit
import System.IO
import System.Process

type FromAddress = String
type ToAddress = String
type Subject = String
type Body = String

sendMail :: FromAddress -> [ToAddress] -> Subject -> Body -> Maybe Body
         -> [Attachment]
         -> IO ()
sendMail from tos subject body maybeHtmlBody attachments
 = do let toField = concat $ intersperse ", " tos
          headers = [mk_header ["From: Builder <" ++ from ++ ">"],
                     mk_header ["To: " ++ toField],
                     mk_header ["Subject: " ++ subject]]
      mail <- flatten headers body maybeHtmlBody attachments
      (hin, hout, herr, ph) <- runInteractiveProcess "/usr/sbin/sendmail"
                                                     ["-ti", "-f", from]
                                                     Nothing Nothing
      vout <- newEmptyMVar
      verr <- newEmptyMVar
      forkIO $ hGetContents hout >>= putMVar vout
      forkIO $ hGetContents herr >>= putMVar verr
      hPutStr hin mail
      hClose hin
      ec <- waitForProcess ph
      sout <- takeMVar vout
      serr <- takeMVar verr
      case ec of
          ExitSuccess
           | null sout && null serr ->
              return ()
           | otherwise ->
              error ("Unexpected output:\n" ++ sout ++ "\n" ++ serr)
          ExitFailure f ->
              error ("Mail failed: " ++ show f)

