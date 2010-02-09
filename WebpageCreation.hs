
module WebpageCreation where

import BuildStep
import ServerMonad
import Utils

import Control.Concurrent.MVar
import System.Directory
import System.Exit
import System.FilePath
import Text.XHtml.Strict

webpageCreator :: WCVar -> IO ()
webpageCreator mv
 = do (user, bn) <- takeMVar mv
      createWebPage user bn
      webpageCreator mv

createWebPage :: User -> BuildNum -> IO ()
createWebPage u bn
 = do let buildsDir = baseDir </> "clients" </> u </> "builds"
          buildDir = buildsDir </> show bn
          stepsDir = buildDir </> "steps"
          webBuildDir = baseDir </> "web/builders" </> u </> show bn
      steps <- getNumericDirectoryContents stepsDir
      createDirectory webBuildDir
      mapM_ (mkStepPage u bn) steps
      -- mkBuildPage steps
      -- mkIndex

mkStepPage :: User -> BuildNum -> BuildStepNum -> IO ()
mkStepPage u bn bsn
 = do let buildDir = baseDir </> "clients" </> u </> "builds" </> show bn
          stepDir = buildDir </> "steps" </> show bsn
          page = baseDir </> "web/builders" </> u </> show bn </> show bsn <.> "html"
      -- XXX Use reader-functions with type sigs?:
      prog <- readFromFile (stepDir </> "prog") :: IO String
      args <- readFromFile (stepDir </> "args") :: IO [String]
      ec <- readFromFile (stepDir </> "exitcode") :: IO ExitCode
      sOut <- readBinaryFile (stepDir </> "stdout")
      sErr <- readBinaryFile (stepDir </> "stderr")
      let description = u ++ ", build " ++ show bn ++ ", step " ++ show bsn
          descriptionHtml = stringToHtml description
          html = header headerHtml
             +++ body bodyHtml
          bodyHtml = h1 descriptionHtml
                 +++ summaryHtml
                 +++ outputHtml
                 +++ resultHtml
          headerHtml = thetitle descriptionHtml
                   +++ (thelink ! [rel "Stylesheet",
                                   thetype "text/css",
                                   href "../../../css/builder.css"])
                           noHtml
          summaryHtml = (thediv ! [theclass "summary"])
                            (linesToHtml ["Program: " ++ show prog,
                                          "Args: " ++ show args])
          outputHtml = (thediv ! [theclass "stdout"]) (stringToHtml sOut)
                   +++ (thediv ! [theclass "stderr"]) (stringToHtml sErr)
          resultHtml = (thediv ! [theclass "result"])
                           (lineToHtml ("Result: " ++ show ec))
          str = renderHtml html
      writeBinaryFile page str

