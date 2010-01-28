
module BuildStep where

data BuildStep = BuildStep {
                     bs_name :: String,
                     bs_command :: FilePath,
                     bs_args :: [String]
                 }
    deriving (Show, Read)

