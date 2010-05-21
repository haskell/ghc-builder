
module Handlelike where

import qualified Data.ByteString.Char8 as BS
import Network.Socket
import OpenSSL.Session
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
    hlGet' h n = let f n' = do bs <- BS.hGet h n'
                               return (BS.unpack bs, BS.length bs)
                 in hlGetLoop f n

instance Handlelike SSL where
    hlPutStrLn' s str = mapM_ doChunk $ chunk (str ++ "\n")
        where chunk "" = []
              chunk xs = case splitAt 1024 xs of
                         (ys, zs) ->
                             ys : chunk zs
              doChunk xs = write s (BS.pack xs)
    hlGetLine' s = do let f acc = do str <- OpenSSL.Session.read s 1
                                     case BS.unpack str of
                                         "\n" -> return (reverse acc)
                                         [c] -> f (c : acc)
                                         _ -> error "XXX hlGetLine' Socket: Bad recv"
                      f ""
    hlGet' s n = let f n' = do bs <- OpenSSL.Session.read s n'
                               return (BS.unpack bs, BS.length bs)
                 in hlGetLoop f n

instance Handlelike Socket where
    hlPutStrLn' s str = do _ <- send s (str ++ "\n")
                           return ()
    -- XXX Efficiency of "recv s 1"!:
    hlGetLine' s = do let f acc = do str <- recv s 1
                                     case str of
                                         "\n" -> return (reverse acc)
                                         [c] -> f (c : acc)
                                         _ -> error "XXX hlGetLine' Socket: Bad recv"
                      f ""
    hlGet' s n = let f n' = do str <- recv s n'
                               return (str, length str)
                 in hlGetLoop f n

hlGetLoop :: (Int -> IO (String, Int)) -> Int -> IO String
hlGetLoop f n
 | n <= 0    = return ""
 | otherwise = do (str, len) <- f n
                  rest <- hlGetLoop f (n - len)
                  return (str ++ rest)

-- XXX This is now rather misnamed:
data HandleOrSsl = Handle Handle
                 | Ssl SSL
                 | Socket Socket

instance Handlelike HandleOrSsl where
    hlPutStrLn' (Handle h) str = hlPutStrLn' h str
    hlPutStrLn' (Ssl    s) str = hlPutStrLn' s str
    hlPutStrLn' (Socket s) str = hlPutStrLn' s str
    hlGetLine' (Handle h) = hlGetLine' h
    hlGetLine' (Ssl    s) = hlGetLine' s
    hlGetLine' (Socket s) = hlGetLine' s
    hlGet' (Handle h) n = hlGet' h n
    hlGet' (Ssl    s) n = hlGet' s n
    hlGet' (Socket s) n = hlGet' s n

