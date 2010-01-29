
module BuildStep where

type BuildInstructions = (BuildNum, [(BuildStepNum, BuildStep)])

type BuildNum = Integer

type BuildStepNum = Integer

data BuildStep = BuildStep {
                     bs_name :: String,
                     bs_subdir :: FilePath,
                     bs_prog :: FilePath,
                     bs_args :: [String]
                 }
    deriving (Show, Read)

