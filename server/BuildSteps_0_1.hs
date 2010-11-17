
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances #-}

module BuildSteps_0_1 (
              toCurrent, fromCurrent
             ) where

import qualified Builder.BuildSteps as BS
import Builder.Utils

class Translate a b | a -> b, b -> a where
    toCurrent :: a -> b
    fromCurrent :: b -> a

instance Translate a b => Translate [a] [b] where
    toCurrent = map toCurrent
    fromCurrent = map fromCurrent

instance Translate Integer Integer where
    toCurrent = id
    fromCurrent = id

instance (Translate a1 b1, Translate a2 b2) =>
         Translate (a1, a2) (b1, b2) where
    toCurrent (x, y) = (toCurrent x, toCurrent y)
    fromCurrent (x, y) = (fromCurrent x, fromCurrent y)

instance Translate BuildInstructions BS.BuildInstructions where
    toCurrent bi = BS.BuildInstructions {
                       BS.bi_instructions = bi_instructions bi,
                       BS.bi_buildNum     = bi_buildNum bi,
                       BS.bi_buildSteps   = toCurrent (bi_buildSteps bi)
                   }

    fromCurrent bi = BuildInstructions {
                         bi_instructions = BS.bi_instructions bi,
                         bi_buildNum     = BS.bi_buildNum bi,
                         bi_buildSteps   = fromCurrent (BS.bi_buildSteps bi)
                     }

instance Translate BuildStep BS.BuildStep where
    toCurrent bs = BS.BuildStep {
                       BS.bs_name   = bs_name   bs,
                       BS.bs_subdir = bs_subdir bs,
                       BS.bs_prog   = bs_prog   bs,
                       BS.bs_args   = bs_args   bs,
                       BS.bs_mailOutput = False
                   }

    fromCurrent bs = BuildStep {
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

