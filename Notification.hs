
module Notification where

import Email
import ServerMonad
import WebpageCreation

import Control.Concurrent.MVar

notifier :: NVar -> IO ()
notifier nv
 = do (user, bn) <- takeMVar nv
      webpage <- createWebPage user bn
      sendEmails user bn webpage
      notifier nv

