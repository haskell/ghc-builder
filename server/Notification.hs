
module Notification where

import Email
import ServerMonad
import WebpageCreation

import Control.Concurrent.MVar

notifier :: Directory -> IO ()
notifier directory
 = do (user, bn) <- takeMVar (dir_notifierVar directory)
      config <- getConfig directory
      webpage <- createWebPage config user bn
      sendEmails config user bn webpage
      notifier directory

