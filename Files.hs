
module Files (
 Root(..),
 removeBuildStepName,
 getMaybeBuildStepName,     putMaybeBuildStepName,
 readBuildStepName,         readMaybeBuildStepName,     writeBuildStepName,
 removeBuildStepSubdir,
 getMaybeBuildStepSubdir,   putMaybeBuildStepSubdir,
 readBuildStepSubdir,       readMaybeBuildStepSubdir,   writeBuildStepSubdir,
 removeBuildStepProg,
 getMaybeBuildStepProg,     putMaybeBuildStepProg,
 readBuildStepProg,         readMaybeBuildStepProg,     writeBuildStepProg,
 removeBuildStepArgs,
 getMaybeBuildStepArgs,     putMaybeBuildStepArgs,
 readBuildStepArgs,         readMaybeBuildStepArgs,     writeBuildStepArgs,
 removeBuildStepExitcode,
 getMaybeBuildStepExitcode, putMaybeBuildStepExitcode,
 readBuildStepExitcode,     readMaybeBuildStepExitcode, writeBuildStepExitcode,
 removeBuildStepOutput,
 getMaybeBuildStepOutput,   putMaybeBuildStepOutput,
 removeBuildResult,
 getMaybeBuildResult,       putMaybeBuildResult,
 readBuildResult,                                       writeBuildResult,
             ) where

import BuildStep
import Utils

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

readBuildStepName :: MonadIO m => Root -> BuildNum -> BuildStepNum -> m String
readBuildStepName root bn bsn = readFromFile $ fpBuildStepName root bn bsn

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
fpBuildStepSubdir root bn bsn = mkPath root (dirBuildStep bn bsn </> "name")

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

readBuildStepSubdir :: MonadIO m
                    => Root -> BuildNum -> BuildStepNum -> m FilePath
readBuildStepSubdir root bn bsn
 = readFromFile $ fpBuildStepSubdir root bn bsn

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

readBuildStepProg :: MonadIO m => Root -> BuildNum -> BuildStepNum -> m String
readBuildStepProg root bn bsn = readFromFile $ fpBuildStepProg root bn bsn

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

readBuildStepArgs :: MonadIO m
                  => Root -> BuildNum -> BuildStepNum -> m [String]
readBuildStepArgs root bn bsn = readFromFile $ fpBuildStepArgs root bn bsn

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

readBuildStepExitcode :: MonadIO m
                      => Root -> BuildNum -> BuildStepNum -> m ExitCode
readBuildStepExitcode root bn bsn
 = readFromFile $ fpBuildStepExitcode root bn bsn

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

fpBuildStepOutput :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepOutput root bn bsn
 = mkPath root (dirBuildStep bn bsn </> "exitcode")

removeBuildStepOutput :: MonadIO m => Root -> BuildNum -> BuildStepNum -> m ()
removeBuildStepOutput root bn bsn
 = liftIO $ ignoreDoesNotExist $ removeFile (fpBuildStepOutput root bn bsn)

getMaybeBuildStepOutput :: MonadIO m
                        => Root -> BuildNum -> BuildStepNum
                        -> m (Maybe String)
getMaybeBuildStepOutput root bn bsn
 = maybeReadBinaryFile (fpBuildStepOutput root bn bsn)

putMaybeBuildStepOutput :: MonadIO m
                        => Root -> BuildNum -> BuildStepNum -> Maybe String
                        -> m ()
putMaybeBuildStepOutput root bn bsn m
 = maybeWriteBinaryFile (fpBuildStepOutput root bn bsn) m

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

readBuildResult :: MonadIO m => Root -> BuildNum -> m Result
readBuildResult root bn = readFromFile $ fpBuildResult root bn

writeBuildResult :: MonadIO m => Root -> BuildNum -> Result -> m ()
writeBuildResult root bn result
 = writeBinaryFile (fpBuildResult root bn) (show result)

