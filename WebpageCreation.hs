
module WebpageCreation where

import BuildStep
import Command
import Files
import ServerMonad
import Utils

import Control.Concurrent.MVar
import Control.Monad
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
      steps <- getSortedNumericDirectoryContents stepsDir
      createDirectory webBuildDir
      mapM_ (mkStepPage u bn) steps
      mkBuildPage u bn steps
      mkIndex u

mkStepPage :: User -> BuildNum -> BuildStepNum -> IO ()
mkStepPage u bn bsn
 = do let root = Server (baseDir </> "clients") u
          buildDir = baseDir </> "clients" </> u </> "builds" </> show bn
          stepDir = buildDir </> "steps" </> show bsn
          page = baseDir </> "web/builders" </> u </> show bn </> show bsn <.> "html"
      stepName <- readBuildStepName root bn bsn
      prog <- readBuildStepProg root bn bsn
      args <- readBuildStepArgs root bn bsn
      ec <- readBuildStepExitcode root bn bsn
      output <- liftM lines $ readBinaryFile (stepDir </> "output")
      let description = u ++ ", build " ++ show bn ++ ", step " ++ show bsn ++ ": " ++ stepName
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
          outputHtml = (pre ! [theclass "output"])
                           (concatHtml $ map doLine output)
          doLine lineStr = case maybeRead lineStr of
                           Just (Stdout line) ->
                               (thediv ! [theclass "stdout"])
                                   (stringToHtml line)
                           Just (Stderr line) ->
                               (thediv ! [theclass "stderr"])
                                   (stringToHtml line)
                           Nothing ->
                               (thediv ! [theclass "panic"])
                                   (stringToHtml lineStr)
          resultHtml = (thediv ! [theclass "result"])
                           (lineToHtml ("Result: " ++ show ec))
          str = renderHtml html
      writeBinaryFile page str

mkBuildPage :: User -> BuildNum -> [BuildStepNum] -> IO ()
mkBuildPage u bn bsns
 = do let root = Server (baseDir </> "clients") u
          page = baseDir </> "web/builders" </> u </> show bn <.> "html"
          mkLink bsn = do stepName <- readBuildStepName root bn bsn
                          ec <- readBuildStepExitcode root bn bsn
                          let linkClass = case ec of
                                          ExitSuccess -> "success"
                                          _ -> "failure"
                          return ((anchor ! [href (show bn </> show bsn <.> "html"),
                                             theclass linkClass])
                                     (stringToHtml (show bsn ++ ": " ++ stepName)))
      links <- mapM mkLink bsns
      result <- readBuildResult root bn
      let linkClass = case result of
                      Success -> "success"
                      Failure -> "failure"
                      Incomplete -> "incomplete"
      let description = u ++ ", build " ++ show bn
          descriptionHtml = stringToHtml description
          html = header headerHtml
             +++ body bodyHtml
          bodyHtml = h1 descriptionHtml
                 +++ ulist (concatHtml (map li links))
                 +++ (paragraph ! [theclass linkClass])
                         (stringToHtml $ show result)
          headerHtml = thetitle descriptionHtml
                   +++ (thelink ! [rel "Stylesheet",
                                   thetype "text/css",
                                   href "../../css/builder.css"])
                           noHtml
          str = renderHtml html
      writeBinaryFile page str

-- XXX This should do something:
mkIndex :: User -> IO ()
mkIndex _ = return ()

