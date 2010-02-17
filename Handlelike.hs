
module Handlelike where

class Monad m => HandlelikeM m where
    hlPutStrLn :: String -> m ()
    hlGetLine :: m String
    hlGet :: Int -> m String
