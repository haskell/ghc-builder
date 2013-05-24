
module Notification where

import Email
import ServerMonad
import WebpageCreation

import Control.Concurrent.MVar

notifier :: Directory -> IO ()
notifier directory
 = do (user, bn) <- takeMVar (dir_notifierVar directory)
      config <- getConfig directory
      let myWarn = warn' directory (ClientThread (User user))
      webpage <- createWebPage myWarn config user bn
      sendEmails config user bn webpage
      notifier directory

