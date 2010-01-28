
module Utils where

import qualified Data.ByteString.Lazy.Char8 as BS
import System.Exit
import System.IO

die :: String -> IO a
die msg = do hPutStrLn stderr msg
             exitWith (ExitFailure 1)

maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
                [(x, "")] -> Just x
                _ -> Nothing

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile fp str = withBinaryFile fp WriteMode (\h -> hPutStr h str)

readSizedThing :: Read a => Handle -> IO a
readSizedThing h
 = do sizeStr <- hGetLine h
      case maybeRead sizeStr of
          Nothing ->
              die ("Bad size: " ++ show sizeStr)
          Just size ->
              do bs <- BS.hGet h size
                 let str = BS.unpack bs
                 case maybeRead $ BS.unpack bs of
                     Nothing ->
                         die ("Unreadable data: " ++ show str)
                     Just x ->
                         do line <- hGetLine h
                            if null line
                                then return x
                                else die ("Stuff after data: " ++ show line)

sendSizedThing :: Show a => Handle -> a -> IO ()
sendSizedThing h x = do let str = show x
                        hPutStrLn h $ show $ length str
                        hPutStrLn h str

