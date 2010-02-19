
module Files (Root(..),
              readBuildStepName,        writeBuildStepName,
              readBuildStepProg,        writeBuildStepProg,
              readBuildStepArgs,        writeBuildStepArgs,
              readBuildStepExitcode,    writeBuildStepExitcode,
              readBuildResult,          writeBuildResult,
             ) where

import BuildStep
import Utils

import Control.Monad.Trans
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

fpBuildStepName :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepName root bn bsn = mkPath root (dirBuildStep bn bsn </> "name")

readBuildStepName :: MonadIO m => Root -> BuildNum -> BuildStepNum -> m String
readBuildStepName root bn bsn = readFromFile $ fpBuildStepName root bn bsn

writeBuildStepName :: MonadIO m => Root -> BuildNum -> BuildStepNum -> String -> m ()
writeBuildStepName root bn bsn n
 = writeBinaryFile (fpBuildStepName root bn bsn) (show n)

fpBuildStepProg :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepProg root bn bsn = mkPath root (dirBuildStep bn bsn </> "prog")

readBuildStepProg :: MonadIO m => Root -> BuildNum -> BuildStepNum -> m String
readBuildStepProg root bn bsn = readFromFile $ fpBuildStepProg root bn bsn

writeBuildStepProg :: MonadIO m => Root -> BuildNum -> BuildStepNum -> String -> m ()
writeBuildStepProg root bn bsn prog
 = writeBinaryFile (fpBuildStepProg root bn bsn) (show prog)

fpBuildStepArgs :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepArgs root bn bsn = mkPath root (dirBuildStep bn bsn </> "args")

readBuildStepArgs :: MonadIO m => Root -> BuildNum -> BuildStepNum -> m [String]
readBuildStepArgs root bn bsn = readFromFile $ fpBuildStepArgs root bn bsn

writeBuildStepArgs :: MonadIO m => Root -> BuildNum -> BuildStepNum -> [String] -> m ()
writeBuildStepArgs root bn bsn args
 = writeBinaryFile (fpBuildStepArgs root bn bsn) (show args)

fpBuildStepExitcode :: Root -> BuildNum -> BuildStepNum -> FilePath
fpBuildStepExitcode root bn bsn = mkPath root (dirBuildStep bn bsn </> "exitcode")

readBuildStepExitcode :: MonadIO m => Root -> BuildNum -> BuildStepNum -> m ExitCode
readBuildStepExitcode root bn bsn = readFromFile $ fpBuildStepExitcode root bn bsn

writeBuildStepExitcode :: MonadIO m => Root -> BuildNum -> BuildStepNum -> ExitCode -> m ()
writeBuildStepExitcode root bn bsn exitcode
 = writeBinaryFile (fpBuildStepExitcode root bn bsn) (show exitcode)

-- Stuff in each build

fpBuildResult :: Root -> BuildNum -> FilePath
fpBuildResult root bn = mkPath root ("builds" </> show bn </> "result")

readBuildResult :: MonadIO m => Root -> BuildNum -> m Result
readBuildResult root bn = readFromFile $ fpBuildResult root bn

writeBuildResult :: MonadIO m => Root -> BuildNum -> Result -> m ()
writeBuildResult root bn result
 = writeBinaryFile (fpBuildResult root bn) (show result)

