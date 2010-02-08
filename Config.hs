
module Config (buildSteps, clients) where

import BuildStep
import Utils

buildSteps :: [BuildStep]
buildSteps = [BuildStep {
                  bs_name = "Test build step",
                  bs_subdir = ".",
                  bs_prog = "/bin/mkdir",
                  bs_args = ["build"]
              },
              BuildStep {
                  bs_name = "Second test build step",
                  bs_subdir = "build",
                  bs_prog = "/bin/echo",
                  bs_args = ["argx1", "argx2", "argx3"]
              },
              BuildStep {
                  bs_name = "Third test build step",
                  bs_subdir = "build",
                  bs_prog = "/bin/pwd",
                  bs_args = []
              }]

clients :: [(String, UserInfo)]
clients = [("foo", mkUserInfo "mypass" (Timed (mkTime 2 0))),
           ("bar", mkUserInfo "mypass" Continuous)]

