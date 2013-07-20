
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
import System.Posix.Files
import Text.XHtml.Strict

createWebPage :: (String -> IO ()) -> Config -> User -> BuildNum -> IO String
createWebPage myWarn config u bn
 = do let urlRoot = config_urlRoot config
          usersDir = baseDir </> "clients"
          root = Server usersDir u
          webRootDir = baseDir </> "web/builders"
          webBuilderDir = webRootDir </> u
          webBuildDir = webBuilderDir </> show bn
      steps <- getBuildStepNumbers root bn
               `onDoesNotExist`
               return []
      createDirectory webBuildDir
      createSymbolicLink
          ("../../../../clients/" ++ u ++ "/builds/" ++ show bn ++ "/files")
          (webBuildDir </> "files")
      symlinkUploads root u bn steps
      mapM_ (mkStepPage root u bn) steps
      (relPage, result) <- mkBuildPage root config u bn steps
      mEndTime <- getEndTime' root bn steps
      mkBuilderIndex myWarn root webBuilderDir u bn mEndTime result
      mkIndex myWarn usersDir webRootDir u bn mEndTime result
      return (urlRoot </> relPage)

symlinkUploads :: Root -> User -> BuildNum -> [BuildStepNum] -> IO ()
symlinkUploads root u bn bsns
    = do mFileUploadeds <- mapM (getMaybeBuildStepFileUploaded root bn) bsns
         case catMaybes mFileUploadeds of
             [] -> return ()
             fns ->
                 do let dir = baseDir </> "web/uploads" </> u
                        f fn = createSymbolicLink
                                   ("../../../clients/" ++ u ++ "/builds/" ++ show bn ++ "/files/" ++ fn)
                                   (dir </> fn)
                    createDirectoryIfMissing True dir
                    mapM_ f fns

mkStepPage :: Root -> User -> BuildNum -> BuildStepNum -> IO ()
mkStepPage root u bn bsn
 = do let page = baseDir </> "web/builders" </> u </> show bn </> show bsn <.> "html"
          maybeToHtmlWith _ Nothing  = (thespan ! [theclass "missing"])
                                           (stringToHtml "Missing")
          maybeToHtmlWith f (Just x) = stringToHtml (f x)
          maybeToHtml = maybeToHtmlWith id
          maybeToShowHtml :: Show a => Maybe a -> Html
          maybeToShowHtml = maybeToHtmlWith show
          maybeToTimeHtml = maybeToHtmlWith endTimeToString
      mstepName     <- readMaybeBuildStepName        root bn bsn
      msubdir       <- readMaybeBuildStepSubdir      root bn bsn
      mprog         <- readMaybeBuildStepProg        root bn bsn
      margs         <- readMaybeBuildStepArgs        root bn bsn
      mec           <- readMaybeBuildStepExitcode    root bn bsn
      mStartTime    <- readMaybeBuildStepStartTime   root bn bsn
      mEndTime      <- readMaybeBuildStepEndTime     root bn bsn
      uploadHtml    <- mkUpload True                 root bn bsn
      outputHtml    <- getOutputHtml                 root bn bsn
      let descriptionHtml = stringToHtml (u ++ ", build " ++ show bn ++ ", step " ++ show bsn ++ ": ") +++ maybeToHtml mstepName
          html = header headerHtml
             +++ body bodyHtml
          bodyHtml = h1 descriptionHtml
                 +++ summaryHtml
                 +++ outputHtml
                 +++ resultHtml
                 +++ uploadHtml
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
      uploads <- mapM (mkUpload False root bn) bsns
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
                 +++ concatHtml uploads
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

mkUpload :: Bool -> Root -> BuildNum -> BuildStepNum -> IO Html
mkUpload inBuildDir root bn bsn
    = do mFileUploaded <- getMaybeBuildStepFileUploaded root bn bsn
         return $ case mFileUploaded of
                  Just fn ->
                      let relFn = if inBuildDir then             "files/" ++ fn
                                                else show bn ++ "/files/" ++ fn
                      in (thediv ! [theclass "upload"])
                             (stringToHtml "Upload: " +++
                              (anchor ! [href relFn]) (stringToHtml fn))
                  Nothing -> noHtml

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

data BuilderIndexData
         = BuilderIndexData {
               bidNext :: BuildNum,
               bidBuildResults :: [(BuildNum, Maybe EndTime, Result)]
           }
    deriving (Show, Read)

mkBuilderIndex :: (String -> IO ()) -> Root -> FilePath -> User -> BuildNum
               -> Maybe EndTime -> Result
               -> IO ()
mkBuilderIndex myWarn root webBuilderDir u bn mEndTime result
 = do let builderIndexPage = webBuilderDir </> "index.html"
          builderIndexDataFile = webBuilderDir </> "index.dat"
      mBuilderIndexData <- maybeReadFromFile builderIndexDataFile
      buildResults <- case mBuilderIndexData of
                      Just i
                       | bidNext i == bn ->
                          return ((bn, mEndTime, result) : bidBuildResults i)
                      _ ->
                       do myWarn ("Failed to read " ++ show builderIndexDataFile
                               ++ ". Recreating it.")
                          bns <- getBuildNumbers root
                          mapM (\bn' -> do res <- readBuildResult root bn'
                                           met <- getEndTime root bn'
                                           return (bn', met, res))
                               (reverse bns)
      let builderIndexData' = BuilderIndexData {
                                  bidNext = bn + 1,
                                  bidBuildResults = buildResults
                              }
          html = mkBuilderIndexHtml u buildResults
      writeToFile builderIndexDataFile builderIndexData'
      writeBinaryFile builderIndexPage $ renderHtml html

mkBuilderIndexHtml :: User -> [(BuildNum, Maybe EndTime, Result)] -> Html
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
          links = [ (anchor ! [href url, theclass ("nonwrapped " ++ resultToLinkClass res)])
                        text
                  | (bn, met, res) <- xs
                  , let url = show bn <.> "html"
                        text = stringToHtml (show bn ++ ": " ++ show res)
                           +++ br
                           +++ mEndTimeToString met
                  ]

data IndexData = IndexData {
                     idRecentResults :: [(User, [(BuildNum, Maybe EndTime, Result)])]
                 }
    deriving (Show, Read)

-- The indexWidth is how many previous builds we show the result of in
-- the main index
indexWidth :: Int
indexWidth = 10

mkIndex :: (String -> IO ()) -> FilePath -> FilePath
        -> User -> BuildNum -> Maybe EndTime -> Result
        -> IO ()
mkIndex myWarn usersDir webDir u bn mEndTime result
 = do let indexPage = webDir </> "index.html"
          indexDataFile = webDir </> "index.dat"
      mIndexData <- maybeReadFromFile indexDataFile
      indexData <- case mIndexData of
                   Just (IndexData i) ->
                       do let (mine, others) = partition ((u ==) . fst) i
                              myOne = case mine of
                                      [] -> []
                                      ((_, x) : _) -> x
                                      -- If the list has > 1 element
                                      -- then something's gone wrong,
                                      -- but let's not worry about that
                              myOne' = take indexWidth ((bn, mEndTime, result) : myOne)
                          return ((u, myOne') : others)
                   _ ->
                       do myWarn ("Failed to read " ++ show indexDataFile
                               ++ ". Recreating it.")
                          regenerateIndexData usersDir
      let html = mkIndexHtml indexData
      writeToFile indexDataFile (IndexData indexData)
      writeBinaryFile indexPage $ renderHtml html

regenerateIndexData :: FilePath
                    -> IO [(User, [(BuildNum, Maybe EndTime, Result)])]
regenerateIndexData usersDir
    = do let doUser user = do let root = Server usersDir user
                              bns <- getBuildNumbers root
                              let bns' = take indexWidth (reverse bns)
                              xs <- mapM (doBuild root) bns'
                              return (user, xs)
             doBuild root bn = do res <- readBuildResult root bn
                                  met <- getEndTime root bn
                                  return (bn, met, res)
         users <- getInterestingDirectoryContents usersDir
         mapM doUser users

mkIndexHtml :: [(User, [(BuildNum, Maybe EndTime, Result)])] -> Html
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
                               mkCells u ys)
                         | (u, ys) <- xs
                         , let uLink = (anchor ! [href (u </> "index.html")])
                                           (stringToHtml u)
                               lastResult = case ys of
                                            [] -> Incomplete
                                            (_, _, res) : _ -> res
                               lastResultClass = resultToLinkClass lastResult
                         ]
          mkCells u bnresults = [ mkCell u bn met result
                                | (bn, met, result) <- bnresults ]
          mkCell u bn met res
              = (td ! [theclass ("nonwrapped " ++ resultToLinkClass res)])
              $ (anchor ! [href (u </> show bn <.> "html")])
                    (stringToHtml (show bn ++ ": " ++ show res)
                 +++ br
                 +++ stringToHtml (mEndTimeToString met))

getEndTime :: Root -> BuildNum -> IO (Maybe EndTime)
getEndTime root bn = do steps <- getBuildStepNumbers root bn
                                 `onDoesNotExist`
                                 return []
                        getEndTime' root bn steps

getEndTime' :: Root -> BuildNum -> [BuildStepNum] -> IO (Maybe EndTime)
getEndTime' root bn steps = f (reverse steps)
    where f [] = return Nothing
          f (s : ss) = do met <- readMaybeBuildStepEndTime root bn s
                          case met of
                              Just et -> return (Just et)
                              Nothing -> f ss

resultToLinkClass :: Result -> String
resultToLinkClass Success = "success"
resultToLinkClass Failure = "failure"
resultToLinkClass Incomplete = "incomplete"

mEndTimeToString :: Maybe EndTime -> String
mEndTimeToString Nothing = "<unknown>"
mEndTimeToString (Just et) = endTimeToString et

endTimeToString :: EndTime -> String
endTimeToString = show . posixSecondsToUTCTime . fromInteger

