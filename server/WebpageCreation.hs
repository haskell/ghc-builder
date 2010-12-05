
module WebpageCreation where

import ServerMonad

import Builder.Command
import Builder.Config
import Builder.Files
import Builder.Utils

import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import System.Directory
import System.Exit
import System.FilePath
import Text.XHtml.Strict

createWebPage :: Config -> User -> BuildNum -> IO String
createWebPage config u bn
 = do let urlRoot = config_urlRoot config
          buildsDir = baseDir </> "clients" </> u </> "builds"
          buildDir = buildsDir </> show bn
          stepsDir = buildDir </> "steps"
          webBuildDir = baseDir </> "web/builders" </> u </> show bn
      steps <- getSortedNumericDirectoryContents stepsDir
               `onDoesNotExist`
               return []
      createDirectory webBuildDir
      mapM_ (mkStepPage u bn) steps
      relPage <- mkBuildPage config u bn steps
      mkIndex u
      return (urlRoot </> relPage)

mkStepPage :: User -> BuildNum -> BuildStepNum -> IO ()
mkStepPage u bn bsn
 = do let root = Server (baseDir </> "clients") u
          page = baseDir </> "web/builders" </> u </> show bn </> show bsn <.> "html"
          maybeToHtmlWith _ Nothing  = (thespan ! [theclass "missing"])
                                           (stringToHtml "Missing")
          maybeToHtmlWith f (Just x) = stringToHtml (f x)
          maybeToHtml = maybeToHtmlWith id
          maybeToShowHtml :: Show a => Maybe a -> Html
          maybeToShowHtml = maybeToHtmlWith show
          maybeToTimeHtml = maybeToHtmlWith
                                (show . posixSecondsToUTCTime . fromInteger)
      mstepName  <- readMaybeBuildStepName      root bn bsn
      msubdir    <- readMaybeBuildStepSubdir    root bn bsn
      mprog      <- readMaybeBuildStepProg      root bn bsn
      margs      <- readMaybeBuildStepArgs      root bn bsn
      mec        <- readMaybeBuildStepExitcode  root bn bsn
      mStartTime <- readMaybeBuildStepStartTime root bn bsn
      mEndTime   <- readMaybeBuildStepEndTime   root bn bsn
      outputHtml <- getOutputHtml              root bn bsn
      let descriptionHtml = stringToHtml (u ++ ", build " ++ show bn ++ ", step " ++ show bsn ++ ": ") +++ maybeToHtml mstepName
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
          summaryHtml = (thediv ! [theclass "summary"]) summaryLines
          summaryLines = brLines [
              stringToHtml "Program: "    +++ maybeToShowHtml mprog,
              stringToHtml "Args: "       +++ maybeToShowHtml margs,
              stringToHtml "Subdir: "     +++ maybeToShowHtml msubdir,
              stringToHtml "Start time: " +++ maybeToTimeHtml mStartTime,
              stringToHtml "End time: "   +++ maybeToTimeHtml mEndTime]
          brLines = concatHtml . intersperse br
          resultHtml = (thediv ! [theclass "result"])
                           (stringToHtml "Result: " +++ maybeToShowHtml mec)
          str = renderHtml html
      writeBinaryFile page str

mkBuildPage :: Config -> User -> BuildNum -> [BuildStepNum] -> IO String
mkBuildPage config u bn bsns
 = do let root = Server (baseDir </> "clients") u
          relPage = "builders" </> u </> show bn <.> "html"
          page = baseDir </> "web" </> relPage
      links <- mapM (mkLink root bn) bsns
      result <- readBuildResult root bn
      outputs <- mapM (mkOutput root bn) bsns
      let linkClass = case result of
                      Success -> "success"
                      Failure -> "failure"
                      Incomplete -> "incomplete"
          builderDescription = case lookup u (config_clients config) of
                               Just ui -> " (" ++ ui_description ui ++ ")"
                               Nothing -> ""
          description = u ++ builderDescription ++ ", build " ++ show bn
          descriptionHtml = stringToHtml description
          html = header headerHtml
             +++ body bodyHtml
          bodyHtml = h1 descriptionHtml
                 +++ ulist (concatHtml (map li links))
                 +++ (paragraph ! [theclass linkClass])
                         (stringToHtml $ show result)
                 +++ concatHtml outputs
          headerHtml = thetitle descriptionHtml
                   +++ (thelink ! [rel "Stylesheet",
                                   thetype "text/css",
                                   href "../../css/builder.css"])
                           noHtml
          str = renderHtml html
      writeBinaryFile page str
      return relPage

mkLink :: Root -> BuildNum -> BuildStepNum -> IO Html
mkLink root bn bsn
    = do mStepName <- readMaybeBuildStepName root bn bsn
         mec <- readMaybeBuildStepExitcode root bn bsn
         let stepName = fromMaybe "<<name not found>>" mStepName
             url = show bn </> show bsn <.> "html"
             linkClass = case mec of
                         Just ExitSuccess -> "success"
                         _ -> "failure"
         return ((anchor ! [href url, theclass linkClass])
                    (stringToHtml (show bsn ++ ": " ++ stepName)))

mkOutput :: Root -> BuildNum -> BuildStepNum -> IO Html
mkOutput root bn bsn
    = do mMailOutput <- readMaybeBuildStepMailOutput root bn bsn
         case mMailOutput of
             Just True ->
                 do mStepName <- readMaybeBuildStepName root bn bsn
                    outputHtml <- getOutputHtml root bn bsn
                    let stepName = fromMaybe "<<name not found>>" mStepName
                        output = hr
                             +++ h2 (stringToHtml stepName)
                             +++ outputHtml
                    return output
             _ -> return noHtml

getOutputHtml :: Root -> BuildNum -> BuildStepNum -> IO Html
getOutputHtml root bn bsn
    = do moutput <- getMaybeBuildStepOutput root bn bsn
         let output = case moutput of
                      Just x -> lines x
                      Nothing -> []
             outputHtml = (pre ! [theclass "output"])
                              (concatHtml $ map doLine output)
             doLine lineStr = case maybeReadSpace lineStr of
                              Just (Stdout line) ->
                                  (thediv ! [theclass "stdout"])
                                      (stringToHtml line)
                              Just (Stderr line) ->
                                  (thediv ! [theclass "stderr"])
                                      (stringToHtml line)
                              Nothing ->
                                  (thediv ! [theclass "panic"])
                                      (stringToHtml lineStr)
         return outputHtml

-- XXX This should do something:
mkIndex :: User -> IO ()
mkIndex _ = return ()

