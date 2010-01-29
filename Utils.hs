
module Utils where

import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory
import System.Exit
import System.IO

die :: String -> IO a
die msg = do hPutStrLn stderr msg
             exitWith (ExitFailure 1)

maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
                [(x, "")] -> Just x
                _ -> Nothing

readBinaryFile :: FilePath -> IO String
readBinaryFile fp = do h <- openBinaryFile fp ReadMode
                       hGetContents h

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile fp str = withBinaryFile fp WriteMode (\h -> hPutStr h str)

readSizedThing :: Read a => Handle -> IO a
readSizedThing h
 = do str <- getSizedThing h
      case maybeRead str of
          Nothing ->
              die ("Unreadable data: " ++ show str)
          Just x ->
              return x

sendSizedThing :: Show a => Handle -> a -> IO ()
sendSizedThing h x = putSizedThing h (show x)

getSizedThing :: Handle -> IO String
getSizedThing h
 = do sizeStr <- hGetLine h
      case maybeRead sizeStr of
          Nothing ->
              die ("Bad size: " ++ show sizeStr)
          Just size ->
              do bs <- BS.hGet h size
                 line <- hGetLine h
                 if null line
                     then return $ BS.unpack bs
                     else die ("Stuff after data: " ++ show line)

putSizedThing :: Handle -> String -> IO ()
putSizedThing h str = do hPutStrLn h $ show $ length str
                         hPutStrLn h str

readFromFile :: Read a => FilePath -> IO a
readFromFile fp = do xs <- readBinaryFile fp
                     case maybeRead xs of
                         Nothing ->
                             die "Couldn't read from file"
                         Just x ->
                             return x

writeToFile :: Show a => FilePath -> a -> IO ()
writeToFile fp x = writeBinaryFile fp (show x)

getInterestingDirectoryContents :: FilePath -> IO [FilePath]
getInterestingDirectoryContents fp = do xs <- getDirectoryContents fp
                                        return $ filter interesting xs
    where interesting "."  = False
          interesting ".." = False
          interesting _    = True

