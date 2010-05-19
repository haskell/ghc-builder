
module Email where

import Command
import Config
import Files
import SendMail
import ServerMonad
import Utils

import Codec.MIME.String.Headers
import Control.Monad
import Data.Maybe
import System.Exit
import System.FilePath

sendEmails :: User -> BuildNum -> String -> IO ()
sendEmails u bn url
 = do let buildsDir = baseDir </> "clients" </> u </> "builds"
          buildDir = buildsDir </> show bn
          stepsDir = buildDir </> "steps"
          root = Server (baseDir </> "clients") u
          mkStep bsn = do stepName <- readBuildStepName root bn bsn
                          ec <- readBuildStepExitcode root bn bsn
                          case ec of
                              ExitSuccess ->
                                  return ([stepName, "Success"],
                                          Nothing)
                              ExitFailure n ->
                                  do moutput <- getMaybeBuildStepOutput root bn bsn
                                     let doLine x = case maybeReadSpace x of
                                                    Nothing -> x
                                                    Just (Stdout str) -> str
                                                    Just (Stderr str) -> str
                                         lastFew = case moutput of
                                                   Nothing -> ""
                                                   Just x ->
                                                       unlines $ map doLine $ lastN 30 $ lines x
                                         contentType = ContentType "text" "plain" [Parameter "charset" "ISO-8859-1"]
                                         attachment = (lastFew, "step." ++ stepName ++ ".txt", Just contentType)
                                     return ([stepName, "Failure: " ++ show n],
                                             Just attachment)
      bsns <- getSortedNumericDirectoryContents stepsDir
      steps <- mapM mkStep bsns
      result <- readBuildResult root bn
      let (stepDescrs, maybeAttachments) = unzip steps
          buildResult = case result of
                        Success -> "Build succeeded"
                        Failure -> "Build failed"
                        Incomplete -> "Build incomplete"
          description = u ++ ", build " ++ show bn
          link = "Details: " ++ url
          bodyLines = [description,
                       "",
                       buildResult,
                       link,
                       ""]
                   ++ showTable [rPad, noPad]
                                stepDescrs
                   ++ ["",
                       buildResult,
                       link,
                       ""]
          subject = description ++ ", " ++ show result
          body = unlines bodyLines
      unless (null emailAddresses) $
          sendMail fromAddress emailAddresses subject
                   body Nothing
                   (catMaybes maybeAttachments)

