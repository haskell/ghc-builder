
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances #-}

module BuildSteps_0_1 (
              to_0_2, from_0_2
             ) where

import qualified BuildSteps_0_2 as BS
import Builder.Utils

class Translate a b | a -> b, b -> a where
    to_0_2 :: a -> b
    from_0_2 :: b -> a

instance Translate a b => Translate [a] [b] where
    to_0_2 = map to_0_2
    from_0_2 = map from_0_2

instance Translate Integer Integer where
    to_0_2 = id
    from_0_2 = id

instance (Translate a1 b1, Translate a2 b2) =>
         Translate (a1, a2) (b1, b2) where
    to_0_2 (x, y) = (to_0_2 x, to_0_2 y)
    from_0_2 (x, y) = (from_0_2 x, from_0_2 y)

instance Translate BuildInstructions BS.BuildInstructions where
    to_0_2 bi = BS.BuildInstructions {
                    BS.bi_instructions = bi_instructions bi,
                    BS.bi_buildNum     = bi_buildNum bi,
                    BS.bi_buildSteps   = to_0_2 (bi_buildSteps bi)
                }

    from_0_2 bi = BuildInstructions {
                      bi_instructions = BS.bi_instructions bi,
                      bi_buildNum     = BS.bi_buildNum bi,
                      bi_buildSteps   = from_0_2 (BS.bi_buildSteps bi)
                  }

instance Translate BuildStep BS.BuildStep where
    to_0_2 bs = BS.BuildStep {
                    BS.bs_name       = bs_name   bs,
                    BS.bs_subdir     = bs_subdir bs,
                    BS.bs_prog       = bs_prog   bs,
                    BS.bs_args       = bs_args   bs,
                    BS.bs_mailOutput = False
                }

    from_0_2 bs = BuildStep {
                      bs_name   = BS.bs_name   bs,
                      bs_subdir = BS.bs_subdir bs,
                      bs_prog   = BS.bs_prog   bs,
                      bs_args   = BS.bs_args   bs
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
                     bs_args :: [String]
                 }
    deriving (Show, Read)

