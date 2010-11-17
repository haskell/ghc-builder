
module Builder.BuildSteps (
              BuildInstructions(..), BuildStep(..)
             ) where

import Builder.Utils

data BuildInstructions = BuildInstructions {
                             bi_instructions :: Instructions,
                             bi_buildNum :: BuildNum,
                             bi_buildSteps :: [(BuildStepNum, BuildStep)]
                         }
    deriving (Show, Read)

data BuildStep = BuildStep {
                     bs_name :: String,
                     bs_subdir :: FilePath,
                     bs_prog :: FilePath,
                     bs_args :: [String],
                     bs_mailOutput :: Bool
                 }
    deriving (Show, Read)

