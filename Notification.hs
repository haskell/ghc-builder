
module Notification where

import ServerMonad
import WebpageCreation

import Control.Concurrent.MVar

notifier :: NVar -> IO ()
notifier nv
 = do (user, bn) <- takeMVar nv
      createWebPage user bn
      notifier nv

