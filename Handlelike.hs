
module Handlelike where

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO

class Monad m => HandlelikeM m where
    hlPutStrLn :: String -> m ()
    hlGetLine :: m String
    hlGet :: Int -> m String

class Handlelike h where
    hlPutStrLn' :: h -> String -> IO ()
    hlGetLine' :: h -> IO String
    hlGet' :: h -> Int -> IO String

instance Handlelike Handle where
    hlPutStrLn' h str = hPutStrLn h str
    hlGetLine' h = hGetLine h
    hlGet' h n = liftM BS.unpack $ BS.hGet h n
