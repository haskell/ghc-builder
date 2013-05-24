
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances #-}

module BuildSteps_0_2 (
              to_0_3, from_0_3,
              BuildInstructions(..), BuildStep(..),
             ) where

import qualified Builder.BuildSteps as BS
import Builder.Utils

class Translate a b | a -> b, b -> a where
    to_0_3 :: a -> b
    from_0_3 :: b -> a

instance Translate a b => Translate [a] [b] where
    to_0_3 = map to_0_3
    from_0_3 = map from_0_3

instance Translate Integer Integer where
    to_0_3 = id
    from_0_3 = id

instance (Translate a1 b1, Translate a2 b2) =>
         Translate (a1, a2) (b1, b2) where
    to_0_3 (x, y) = (to_0_3 x, to_0_3 y)
    from_0_3 (x, y) = (from_0_3 x, from_0_3 y)

instance Translate BuildInstructions BS.BuildInstructions where
    to_0_3 bi = BS.BuildInstructions {
                    BS.bi_instructions = bi_instructions bi,
                    BS.bi_buildNum     = bi_buildNum bi,
                    BS.bi_buildSteps   = to_0_3 (bi_buildSteps bi)
                }

    from_0_3 bi = BuildInstructions {
                      bi_instructions = BS.bi_instructions bi,
                      bi_buildNum     = BS.bi_buildNum bi,
                      bi_buildSteps   = from_0_3 (BS.bi_buildSteps bi)
                  }

instance Translate BuildStep BS.BuildStep where
    to_0_3 bs = BS.BuildStep {
                    BS.bs_name       = bs_name       bs,
                    BS.bs_subdir     = bs_subdir     bs,
                    BS.bs_prog       = bs_prog       bs,
                    BS.bs_args       = bs_args       bs,
                    BS.bs_mailOutput = bs_mailOutput bs
                }

    from_0_3 bs = BuildStep {
                      bs_name       = BS.bs_name       bs,
                      bs_subdir     = BS.bs_subdir     bs,
                      bs_prog       = BS.bs_prog       bs,
                      bs_args       = BS.bs_args       bs,
                      bs_mailOutput = BS.bs_mailOutput bs
                  }

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

