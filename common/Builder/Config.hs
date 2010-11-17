
{-# LANGUAGE DeriveDataTypeable #-}

module Builder.Config (
              mkTime, UserInfo(..), mkUserInfo,
              Config(..)
             ) where

import Builder.BuildSteps
import Builder.Utils

import Data.Time.LocalTime
import Data.Typeable

mkTime :: Int -> Int -> TimeOfDay
mkTime hour mins = TimeOfDay {
                       todHour = hour,
                       todMin = mins,
                       todSec = 0
                   }

data UserInfo = UserInfo {
                    ui_password :: String,
                    ui_timezone :: String,
                    ui_buildTime :: BuildTime,
                    ui_buildInstructions :: [BuildStep]
                }
    deriving Typeable

mkUserInfo :: String -> String -> BuildTime -> [BuildStep] -> UserInfo
mkUserInfo pass tz bt bis
    = UserInfo {
          ui_password = pass,
          ui_timezone = tz,
          ui_buildTime = bt,
          ui_buildInstructions = bis
      }

data Config = Config {
                  config_fromAddress :: String,
                  config_emailAddresses :: [String],
                  config_urlRoot :: String,
                  config_clients :: [(String, UserInfo)]
              }
    deriving Typeable

