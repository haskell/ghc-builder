
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
          usersDir = baseDir </> "clients"
          root = Server usersDir u
          buildsDir = usersDir </> u </> "builds"
          buildDir = buildsDir </> show bn
          stepsDir = buildDir </> "steps"
          webRootDir = baseDir </> "web/builders"
          webBuilderDir = webRootDir </> u
          webBuildDir = webBuilderDir </> show bn
      steps <- getSortedNumericDirectoryContents stepsDir
               `onDoesNotExist`
               return []
      createDirectory webBuildDir
      mapM_ (mkStepPage root u bn) steps
      (relPage, result) <- mkBuildPage root config u bn steps
      mkBuilderIndex root webBuilderDir u bn result
      mkIndex usersDir webRootDir u bn result
      return (urlRoot </> relPage)

mkStepPage :: Root -> User -> BuildNum -> BuildStepNum -> IO ()
mkStepPage root u bn bsn
 = do let page = baseDir </> "web/builders" </> u </> show bn </> show bsn <.> "html"
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
      outputHtml <- getOutputHtml               root bn bsn
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

mkBuildPage :: Root -> Config -> User -> BuildNum -> [BuildStepNum]
            -> IO (String, Result)
mkBuildPage root config u bn bsns
 = do let relPage = "builders" </> u </> show bn <.> "html"
          page = baseDir </> "web" </> relPage
      links <- mapM (mkLink root bn) bsns
      result <- readBuildResult root bn
      outputs <- mapM (mkOutput root bn) bsns
      let builderDescription = case lookup u (config_clients config) of
                               Just ui -> " (" ++ ui_description ui ++ ")"
                               Nothing -> ""
          description = u ++ builderDescription ++ ", build " ++ show bn
          descriptionHtml = stringToHtml description
          html = header headerHtml
             +++ body bodyHtml
          bodyHtml = h1 descriptionHtml
                 +++ ulist (concatHtml (map li links))
                 +++ (paragraph ! [theclass (resultToLinkClass result)])
                         (stringToHtml $ show result)
                 +++ concatHtml outputs
          headerHtml = thetitle descriptionHtml
                   +++ (thelink ! [rel "Stylesheet",
                                   thetype "text/css",
                                   href "../../css/builder.css"])
                           noHtml
          str = renderHtml html
      writeBinaryFile page str
      return (relPage, result)

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

data BuilderIndexData = BuilderIndexData {
                            bidNext :: BuildNum,
                            bidBuildResults :: [(BuildNum, Result)]
                        }
    deriving (Show, Read)

mkBuilderIndex :: Root -> FilePath -> User -> BuildNum -> Result -> IO ()
mkBuilderIndex root webBuilderDir u bn result
 = do let builderIndexPage = webBuilderDir </> "index.html"
          builderIndexDataFile = webBuilderDir </> "index.dat"
      mBuilderIndexData <- maybeReadFromFile builderIndexDataFile
      buildResults <- case mBuilderIndexData of
                      Just i
                       | bidNext i == bn -> return $ bidBuildResults i
                      _ ->
                       do warn ("Failed to read " ++ show builderIndexDataFile
                             ++ ". Recreating it.")
                          bns <- getBuildNumbers root
                          mapM (\bn' -> do res <- readBuildResult root bn'
                                           return (bn', res))
                               (reverse bns)
      let buildResults' = (bn, result) : buildResults
          builderIndexData' = BuilderIndexData {
                                  bidNext = bn + 1,
                                  bidBuildResults = buildResults'
                              }
          html = mkBuilderIndexHtml u buildResults'
      writeToFile builderIndexDataFile builderIndexData'
      writeBinaryFile builderIndexPage $ renderHtml html

mkBuilderIndexHtml :: User -> [(BuildNum, Result)] -> Html
mkBuilderIndexHtml u xs = header headerHtml
                      +++ body bodyHtml
    where headerHtml = thetitle descriptionHtml
                   +++ (thelink ! [rel "Stylesheet",
                                   thetype "text/css",
                                   href "../../css/builder.css"])
                           noHtml
          bodyHtml = h1 descriptionHtml
                 +++ ulist (concatHtml (map li links))
          descriptionHtml = stringToHtml u
          links = [ (anchor ! [href url, theclass (resultToLinkClass res)])
                        (stringToHtml (show bn ++ ": " ++ show res))
                  | (bn, res) <- xs
                  , let url = show bn <.> "html"
                  ]

data IndexData = IndexData {
                     idRecentResults :: [(User, [(BuildNum, Result)])]
                 }
    deriving (Show, Read)

-- The indexWidth is how many previous builds we show the result of in
-- the main index
indexWidth :: Int
indexWidth = 10

mkIndex :: FilePath -> FilePath -> User -> BuildNum -> Result -> IO ()
mkIndex usersDir webDir u bn result
 = do let indexPage = webDir </> "index.html"
          indexDataFile = webDir </> "index.dat"
      mIndexData <- maybeReadFromFile indexDataFile
      indexData <- case mIndexData of
                   Just i -> return $ idRecentResults i
                   _ ->
                       do warn ("Failed to read " ++ show indexDataFile
                             ++ ". Recreating it.")
                          regenerateIndexData usersDir
      let (myIndexData, othersIndexData) = partition ((u ==) . fst) indexData
          myIndexDatum = case myIndexData of
                         [] -> []
                         ((_, x) : _) -> x
                         -- If the list has > 1 element then something's
                         -- gone wrong, but let's not worry about that
          indexData' = (u, take indexWidth ((bn, result) : myIndexDatum))
                     : othersIndexData
          html = mkIndexHtml indexData'
      writeToFile indexDataFile indexData'
      writeBinaryFile indexPage $ renderHtml html

regenerateIndexData :: FilePath -> IO [(User, [(BuildNum, Result)])]
regenerateIndexData usersDir
    = do let doUser user = do let root = Server usersDir user
                              bns <- getBuildNumbers root
                              let bns' = take indexWidth (reverse bns)
                              xs <- mapM (doBuild root) bns'
                              return (user, xs)
             doBuild root bn = do res <- readBuildResult root bn
                                  return (bn, res)
         users <- getInterestingDirectoryContents usersDir
         mapM doUser users

mkIndexHtml :: [(User, [(BuildNum, Result)])] -> Html
mkIndexHtml xs = header headerHtml
             +++ body bodyHtml
    where headerHtml = thetitle descriptionHtml
                   +++ (thelink ! [rel "Stylesheet",
                                   thetype "text/css",
                                   href "../css/builder.css"])
                           noHtml
          bodyHtml = h1 descriptionHtml
                 +++ (table ! [border 1])
                         (concatHtml builderTable)
          descriptionHtml = stringToHtml "Builder summary"
          builderTable = [ tr ((td ! [theclass lastResultClass]) uLink +++
                               mkCells u bnresults)
                         | (u, bnresults) <- xs
                         , let uLink = (anchor ! [href (u </> "index.html")])
                                           (stringToHtml u)
                               lastResult = case bnresults of
                                            [] -> Incomplete
                                            (_, res) : _ -> res
                               lastResultClass = resultToLinkClass lastResult
                         ]
          mkCells u bnresults = [ mkCell u bn result
                                | (bn, result) <- bnresults ]
          mkCell u bn res = (td ! [theclass (resultToLinkClass res)])
                          $ (anchor ! [href (u </> show bn <.> "html")])
                          $ stringToHtml (show bn ++ ": " ++ show res)

resultToLinkClass :: Result -> String
resultToLinkClass Success = "success"
resultToLinkClass Failure = "failure"
resultToLinkClass Incomplete = "incomplete"

