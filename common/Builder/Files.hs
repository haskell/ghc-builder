
module Builder.Files (
 Root(..),
 --
 getBuildNumbers, getBuildStepNumbers,
 --
 removeBuildStepName,
 getMaybeBuildStepName,         putMaybeBuildStepName,
 readMaybeBuildStepName,        writeBuildStepName,
 --
 removeBuildStepSubdir,
 getMaybeBuildStepSubdir,       putMaybeBuildStepSubdir,
 readMaybeBuildStepSubdir,      writeBuildStepSubdir,
 --
 removeBuildStepProg,
 getMaybeBuildStepProg,         putMaybeBuildStepProg,
 readMaybeBuildStepProg,        writeBuildStepProg,
 --
 removeBuildStepArgs,
 getMaybeBuildStepArgs,         putMaybeBuildStepArgs,
 readMaybeBuildStepArgs,        writeBuildStepArgs,
 --
 removeBuildStepMailOutput,
 getMaybeBuildStepMailOutput,   putMaybeBuildStepMailOutput,
 readMaybeBuildStepMailOutput,  writeBuildStepMailOutput,
 --
 removeBuildStepExitcode,
 getMaybeBuildStepExitcode,     putMaybeBuildStepExitcode,
 readMaybeBuildStepExitcode,    writeBuildStepExitcode,
 --
 removeBuildStepStartTime,
 getMaybeBuildStepStartTime,    putMaybeBuildStepStartTime,
 readMaybeBuildStepStartTime,   writeBuildStepStartTime,
 --
 removeBuildStepEndTime,
 getMaybeBuildStepEndTime,      putMaybeBuildStepEndTime,
 readMaybeBuildStepEndTime,     writeBuildStepEndTime,
 --
 removeBuildStepOutput,
 getMaybeSizedBuildStepOutput,
 getMaybeBuildStepOutput,       putMaybeBuildStepOutput,
 --
 removeBuildStepFileUploaded,
 getMaybeBuildStepFileUploaded, putMaybeBuildStepFileUploaded,
 --
 removeBuildStepFileUpload,
 getMaybeSizedBuildStepFileUpload,
                                putMaybeBuildStepFileUpload,
 --
 removeBuildResult,
 getMaybeBuildResult,           putMaybeBuildResult,
 readBuildResult,               writeBuildResult,
 --
 removeBuildInstructions,
 getMaybeBuildInstructions,     putMaybeBuildInstructions,
                                writeBuildInstructions,
             ) where

import Builder.Utils

import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import System.Directory
import System.Exit
import System.FilePath

data Root = Client FilePath
          | Server FilePath User

mkPath :: Root -> FilePath -> FilePath
mkPath (Client root)      fp = root </>          fp
mkPath (Server root user) fp = root </> user </> fp

-- Stuff in each build step

dirBuildStep :: BuildNum -> BuildStepNum -> FilePath
dirBuildStep bn bsn = "builds" </> show bn </> "steps" </> show bsn

dirBuildFiles :: BuildNum -> FilePath
dirBuildFiles bn = "builds" </> show bn </> "files"

--

getBuildNumbers :: Root -> IO [BuildNum]
getBuildNumbers root = getSortedNumericDirectoryContents (mkPath root "builds")

getBuildStepNumbers :: Root -> BuildNum -> IO [BuildStepNum]
getBuildStepNumbers root bn = getSortedNumericDirectoryContents (mkPath root ("builds" </> show bn </> "steps"))

--

fpBuildStepName :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepName root bn bsn = mkPath root (dirBuildStep bn bsn </> "name")

removeBuildStepName :: MonadIO m => Root -> BuildNum -> BuildStepNum -> m ()
removeBuildStepName root bn bsn
 = liftIO $ ignoreDoesNotExist $ removeFile (fpBuildStepName root bn bsn)

getMaybeBuildStepName :: MonadIO m
                      => Root -> BuildNum -> BuildStepNum -> m (Maybe String)
getMaybeBuildStepName root bn bsn
 = maybeReadBinaryFile (fpBuildStepName root bn bsn)

putMaybeBuildStepName :: MonadIO m
                      => Root -> BuildNum -> BuildStepNum -> Maybe String
                      -> m ()
putMaybeBuildStepName root bn bsn m
 = maybeWriteBinaryFile (fpBuildStepName root bn bsn) m

readMaybeBuildStepName :: MonadIO m
                       => Root -> BuildNum -> BuildStepNum -> m (Maybe String)
readMaybeBuildStepName root bn bsn
 = maybeReadFromFile $ fpBuildStepName root bn bsn

writeBuildStepName :: MonadIO m
                   => Root -> BuildNum -> BuildStepNum -> String -> m ()
writeBuildStepName root bn bsn n
 = writeBinaryFile (fpBuildStepName root bn bsn) (show n)

--

fpBuildStepSubdir :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepSubdir root bn bsn = mkPath root (dirBuildStep bn bsn </> "subdir")

removeBuildStepSubdir :: MonadIO m => Root -> BuildNum -> BuildStepNum -> m ()
removeBuildStepSubdir root bn bsn
 = liftIO $ ignoreDoesNotExist $ removeFile (fpBuildStepSubdir root bn bsn)

getMaybeBuildStepSubdir :: MonadIO m
                        => Root -> BuildNum -> BuildStepNum -> m (Maybe String)
getMaybeBuildStepSubdir root bn bsn
 = maybeReadBinaryFile (fpBuildStepSubdir root bn bsn)

putMaybeBuildStepSubdir :: MonadIO m
                        => Root -> BuildNum -> BuildStepNum -> Maybe String
                        -> m ()
putMaybeBuildStepSubdir root bn bsn m
 = maybeWriteBinaryFile (fpBuildStepSubdir root bn bsn) m

readMaybeBuildStepSubdir :: MonadIO m
                         => Root -> BuildNum -> BuildStepNum
                         -> m (Maybe FilePath)
readMaybeBuildStepSubdir root bn bsn
 = maybeReadFromFile $ fpBuildStepSubdir root bn bsn

writeBuildStepSubdir :: MonadIO m
                     => Root -> BuildNum -> BuildStepNum -> FilePath -> m ()
writeBuildStepSubdir root bn bsn subdir
 = writeBinaryFile (fpBuildStepSubdir root bn bsn) (show subdir)

--

fpBuildStepProg :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepProg root bn bsn = mkPath root (dirBuildStep bn bsn </> "prog")

removeBuildStepProg :: MonadIO m => Root -> BuildNum -> BuildStepNum -> m ()
removeBuildStepProg root bn bsn
 = liftIO $ ignoreDoesNotExist $ removeFile (fpBuildStepProg root bn bsn)

getMaybeBuildStepProg :: MonadIO m
                      => Root -> BuildNum -> BuildStepNum -> m (Maybe String)
getMaybeBuildStepProg root bn bsn
 = maybeReadBinaryFile (fpBuildStepProg root bn bsn)

putMaybeBuildStepProg :: MonadIO m
                      => Root -> BuildNum -> BuildStepNum -> Maybe String
                      -> m ()
putMaybeBuildStepProg root bn bsn m
 = maybeWriteBinaryFile (fpBuildStepProg root bn bsn) m

readMaybeBuildStepProg :: MonadIO m
                       => Root -> BuildNum -> BuildStepNum
                       -> m (Maybe FilePath)
readMaybeBuildStepProg root bn bsn
 = maybeReadFromFile $ fpBuildStepProg root bn bsn

writeBuildStepProg :: MonadIO m
                   => Root -> BuildNum -> BuildStepNum -> String -> m ()
writeBuildStepProg root bn bsn prog
 = writeBinaryFile (fpBuildStepProg root bn bsn) (show prog)

--

fpBuildStepArgs :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepArgs root bn bsn = mkPath root (dirBuildStep bn bsn </> "args")

removeBuildStepArgs :: MonadIO m => Root -> BuildNum -> BuildStepNum -> m ()
removeBuildStepArgs root bn bsn
 = liftIO $ ignoreDoesNotExist $ removeFile (fpBuildStepArgs root bn bsn)

getMaybeBuildStepArgs :: MonadIO m
                      => Root -> BuildNum -> BuildStepNum -> m (Maybe String)
getMaybeBuildStepArgs root bn bsn
 = maybeReadBinaryFile (fpBuildStepArgs root bn bsn)

putMaybeBuildStepArgs :: MonadIO m
                      => Root -> BuildNum -> BuildStepNum -> Maybe String
                      -> m ()
putMaybeBuildStepArgs root bn bsn m
 = maybeWriteBinaryFile (fpBuildStepArgs root bn bsn) m

readMaybeBuildStepArgs :: MonadIO m
                       => Root -> BuildNum -> BuildStepNum
                       -> m (Maybe [String])
readMaybeBuildStepArgs root bn bsn
 = maybeReadFromFile $ fpBuildStepArgs root bn bsn

writeBuildStepArgs :: MonadIO m
                   => Root -> BuildNum -> BuildStepNum -> [String] -> m ()
writeBuildStepArgs root bn bsn args
 = writeBinaryFile (fpBuildStepArgs root bn bsn) (show args)

--

fpBuildStepMailOutput :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepMailOutput root bn bsn = mkPath root (dirBuildStep bn bsn </> "mailOutput")

removeBuildStepMailOutput :: MonadIO m
                          => Root -> BuildNum -> BuildStepNum -> m ()
removeBuildStepMailOutput root bn bsn
 = liftIO $ ignoreDoesNotExist $ removeFile (fpBuildStepMailOutput root bn bsn)

getMaybeBuildStepMailOutput :: MonadIO m
                            => Root -> BuildNum -> BuildStepNum
                            -> m (Maybe String)
getMaybeBuildStepMailOutput root bn bsn
 = maybeReadBinaryFile (fpBuildStepMailOutput root bn bsn)

putMaybeBuildStepMailOutput :: MonadIO m
                            => Root -> BuildNum -> BuildStepNum -> Maybe String
                            -> m ()
putMaybeBuildStepMailOutput root bn bsn m
 = maybeWriteBinaryFile (fpBuildStepMailOutput root bn bsn) m

readMaybeBuildStepMailOutput :: MonadIO m
                             => Root -> BuildNum -> BuildStepNum
                             -> m (Maybe Bool)
readMaybeBuildStepMailOutput root bn bsn
 = maybeReadFromFile $ fpBuildStepMailOutput root bn bsn

writeBuildStepMailOutput :: MonadIO m
                         => Root -> BuildNum -> BuildStepNum -> Bool -> m ()
writeBuildStepMailOutput root bn bsn args
 = writeBinaryFile (fpBuildStepMailOutput root bn bsn) (show args)

--

fpBuildStepExitcode :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepExitcode root bn bsn
 = mkPath root (dirBuildStep bn bsn </> "exitcode")

removeBuildStepExitcode :: MonadIO m
                        => Root -> BuildNum -> BuildStepNum -> m ()
removeBuildStepExitcode root bn bsn
 = liftIO $ ignoreDoesNotExist $ removeFile (fpBuildStepExitcode root bn bsn)

getMaybeBuildStepExitcode :: MonadIO m
                          => Root -> BuildNum -> BuildStepNum
                          -> m (Maybe String)
getMaybeBuildStepExitcode root bn bsn
 = maybeReadBinaryFile (fpBuildStepExitcode root bn bsn)

putMaybeBuildStepExitcode :: MonadIO m
                          => Root -> BuildNum -> BuildStepNum -> Maybe String
                          -> m ()
putMaybeBuildStepExitcode root bn bsn m
 = maybeWriteBinaryFile (fpBuildStepExitcode root bn bsn) m

readMaybeBuildStepExitcode :: MonadIO m
                           => Root -> BuildNum -> BuildStepNum
                           -> m (Maybe ExitCode)
readMaybeBuildStepExitcode root bn bsn
 = maybeReadFromFile $ fpBuildStepExitcode root bn bsn

writeBuildStepExitcode :: MonadIO m
                       => Root -> BuildNum -> BuildStepNum -> ExitCode -> m ()
writeBuildStepExitcode root bn bsn exitcode
 = writeBinaryFile (fpBuildStepExitcode root bn bsn) (show exitcode)

--

fpBuildStepStartTime :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepStartTime root bn bsn
 = mkPath root (dirBuildStep bn bsn </> "startTime")

removeBuildStepStartTime :: MonadIO m
                         => Root -> BuildNum -> BuildStepNum -> m ()
removeBuildStepStartTime root bn bsn
 = liftIO $ ignoreDoesNotExist $ removeFile (fpBuildStepStartTime root bn bsn)

getMaybeBuildStepStartTime :: MonadIO m
                           => Root -> BuildNum -> BuildStepNum
                           -> m (Maybe String)
getMaybeBuildStepStartTime root bn bsn
 = maybeReadBinaryFile (fpBuildStepStartTime root bn bsn)

putMaybeBuildStepStartTime :: MonadIO m
                           => Root -> BuildNum -> BuildStepNum -> Maybe String
                           -> m ()
putMaybeBuildStepStartTime root bn bsn m
 = maybeWriteBinaryFile (fpBuildStepStartTime root bn bsn) m

readMaybeBuildStepStartTime :: MonadIO m
                            => Root -> BuildNum -> BuildStepNum
                            -> m (Maybe StartTime)
readMaybeBuildStepStartTime root bn bsn
 = maybeReadFromFile $ fpBuildStepStartTime root bn bsn

writeBuildStepStartTime :: MonadIO m
                        => Root -> BuildNum -> BuildStepNum -> StartTime
                        -> m ()
writeBuildStepStartTime root bn bsn exitcode
 = writeBinaryFile (fpBuildStepStartTime root bn bsn) (show exitcode)

--

fpBuildStepEndTime :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepEndTime root bn bsn
 = mkPath root (dirBuildStep bn bsn </> "endTime")

removeBuildStepEndTime :: MonadIO m
                       => Root -> BuildNum -> BuildStepNum -> m ()
removeBuildStepEndTime root bn bsn
 = liftIO $ ignoreDoesNotExist $ removeFile (fpBuildStepEndTime root bn bsn)

getMaybeBuildStepEndTime :: MonadIO m
                         => Root -> BuildNum -> BuildStepNum
                         -> m (Maybe String)
getMaybeBuildStepEndTime root bn bsn
 = maybeReadBinaryFile (fpBuildStepEndTime root bn bsn)

putMaybeBuildStepEndTime :: MonadIO m
                         => Root -> BuildNum -> BuildStepNum -> Maybe String
                         -> m ()
putMaybeBuildStepEndTime root bn bsn m
 = maybeWriteBinaryFile (fpBuildStepEndTime root bn bsn) m

readMaybeBuildStepEndTime :: MonadIO m
                          => Root -> BuildNum -> BuildStepNum
                          -> m (Maybe EndTime)
readMaybeBuildStepEndTime root bn bsn
 = maybeReadFromFile $ fpBuildStepEndTime root bn bsn

writeBuildStepEndTime :: MonadIO m
                      => Root -> BuildNum -> BuildStepNum -> EndTime
                      -> m ()
writeBuildStepEndTime root bn bsn exitcode
 = writeBinaryFile (fpBuildStepEndTime root bn bsn) (show exitcode)

--

fpBuildStepOutput :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepOutput root bn bsn
 = mkPath root (dirBuildStep bn bsn </> "output")

removeBuildStepOutput :: MonadIO m => Root -> BuildNum -> BuildStepNum -> m ()
removeBuildStepOutput root bn bsn
 = liftIO $ ignoreDoesNotExist $ removeFile (fpBuildStepOutput root bn bsn)

getMaybeBuildStepOutput :: MonadIO m
                        => Root -> BuildNum -> BuildStepNum
                        -> m (Maybe String)
getMaybeBuildStepOutput root bn bsn
 = maybeReadBinaryFile (fpBuildStepOutput root bn bsn)

getMaybeSizedBuildStepOutput :: MonadIO m
                             => Root -> BuildNum -> BuildStepNum
                             -> m (Maybe (Integer, String))
getMaybeSizedBuildStepOutput root bn bsn
 = maybeReadSizedBinaryFile (fpBuildStepOutput root bn bsn)

putMaybeBuildStepOutput :: MonadIO m
                        => Root -> BuildNum -> BuildStepNum -> Maybe String
                        -> m ()
putMaybeBuildStepOutput root bn bsn m
 = maybeWriteBinaryFile (fpBuildStepOutput root bn bsn) m

--

fpBuildStepFileUploaded :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepFileUploaded root bn bsn
 = mkPath root (dirBuildStep bn bsn </> "fileUploaded")

removeBuildStepFileUploaded :: MonadIO m
                            => Root -> BuildNum -> BuildStepNum -> m ()
removeBuildStepFileUploaded root bn bsn
 = liftIO $ ignoreDoesNotExist $
   removeFile (fpBuildStepFileUploaded root bn bsn)

getMaybeBuildStepFileUploaded :: MonadIO m
                              => Root -> BuildNum -> BuildStepNum
                              -> m (Maybe String)
getMaybeBuildStepFileUploaded root bn bsn
 = maybeReadBinaryFile (fpBuildStepFileUploaded root bn bsn)

putMaybeBuildStepFileUploaded :: MonadIO m
                              => Root -> BuildNum -> BuildStepNum
                              -> Maybe String
                              -> m ()
putMaybeBuildStepFileUploaded root bn bsn m
 = maybeWriteBinaryFile (fpBuildStepFileUploaded root bn bsn) m

--

fpBuildStepFileUploadClient :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepFileUploadClient root bn bsn
 = mkPath root (dirBuildStep bn bsn </> "fileUpload")

fpBuildStepFileUploadServer :: Root -> BuildNum -> FilePath
                            -> FilePath
fpBuildStepFileUploadServer root bn fn
 = mkPath root (dirBuildFiles bn </> fn)

removeBuildStepFileUpload :: MonadIO m
                          => Root -> BuildNum -> BuildStepNum -> m ()
removeBuildStepFileUpload root bn bsn
 = liftIO $ ignoreDoesNotExist $
   removeFile (fpBuildStepFileUploadClient root bn bsn)

getMaybeSizedBuildStepFileUpload :: MonadIO m
                                 => Root -> BuildNum -> BuildStepNum
                                 -> m (Maybe (Integer, String))
getMaybeSizedBuildStepFileUpload root bn bsn
 = maybeReadSizedBinaryFile (fpBuildStepFileUploadClient root bn bsn)

putMaybeBuildStepFileUpload :: MonadIO m
                            => Root -> BuildNum -> FilePath
                            -> Maybe String
                            -> m ()
putMaybeBuildStepFileUpload root bn fn m
 = maybeWriteBinaryFile (fpBuildStepFileUploadServer root bn fn) m

-- Stuff in each build

fpBuildResult :: Root -> BuildNum -> FilePath
fpBuildResult root bn = mkPath root ("builds" </> show bn </> "result")

removeBuildResult :: MonadIO m => Root -> BuildNum -> m ()
removeBuildResult root bn
 = liftIO $ ignoreDoesNotExist $ removeFile (fpBuildResult root bn)

getMaybeBuildResult :: MonadIO m => Root -> BuildNum -> m (Maybe String)
getMaybeBuildResult root bn
 = maybeReadBinaryFile (fpBuildResult root bn)

putMaybeBuildResult :: MonadIO m => Root -> BuildNum -> Maybe String -> m ()
putMaybeBuildResult root bn m
 = maybeWriteBinaryFile (fpBuildResult root bn) m

readMaybeBuildResult :: MonadIO m => Root -> BuildNum -> m (Maybe Result)
readMaybeBuildResult root bn = maybeReadFromFile $ fpBuildResult root bn

readBuildResult :: MonadIO m => Root -> BuildNum -> m Result
readBuildResult root bn
 = liftM (fromMaybe Incomplete) $ readMaybeBuildResult root bn

writeBuildResult :: MonadIO m => Root -> BuildNum -> Result -> m ()
writeBuildResult root bn result
 = writeBinaryFile (fpBuildResult root bn) (show result)

--

fpBuildInstructions :: Root -> BuildNum -> FilePath
fpBuildInstructions root bn = mkPath root ("builds" </> show bn </> "instructions")

removeBuildInstructions :: MonadIO m => Root -> BuildNum -> m ()
removeBuildInstructions root bn
 = liftIO $ ignoreDoesNotExist $ removeFile (fpBuildInstructions root bn)

getMaybeBuildInstructions :: MonadIO m => Root -> BuildNum -> m (Maybe String)
getMaybeBuildInstructions root bn
 = maybeReadBinaryFile (fpBuildInstructions root bn)

putMaybeBuildInstructions :: MonadIO m => Root -> BuildNum -> Maybe String -> m ()
putMaybeBuildInstructions root bn m
 = maybeWriteBinaryFile (fpBuildInstructions root bn) m

writeBuildInstructions :: MonadIO m => Root -> BuildNum -> Instructions -> m ()
writeBuildInstructions root bn result
 = writeBinaryFile (fpBuildInstructions root bn) (show result)

