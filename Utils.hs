
module Utils where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Time.Clock
import Data.Time.LocalTime
import Prelude hiding (catch)
import System.Directory
import System.Exit
import System.IO
import System.IO.Error hiding (catch)

type User = String
type Pass = String

data Verbosity = Silent | Normal | Verbose | Deafening
    deriving (Eq, Ord, Show, Read)

data Result = Success | Failure | Incomplete
    deriving (Eq, Ord, Show, Read)

die :: MonadIO m => String -> m a
die msg = liftIO $ do hPutStrLn stderr msg
                      exitWith (ExitFailure 1)

maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
                [(x, "")] -> Just x
                _ -> Nothing

readBinaryFile :: MonadIO m => FilePath -> m String
readBinaryFile fp = do h <- liftIO $ openBinaryFile fp ReadMode
                       liftIO $ hGetContents h

writeBinaryFile :: MonadIO m => FilePath -> String -> m ()
writeBinaryFile fp str
    = liftIO $ withBinaryFile fp WriteMode (\h -> hPutStr h str)

readSizedThing :: (MonadIO m, Read a) => Handle -> m a
readSizedThing h
 = do str <- getSizedThing h
      case maybeRead str of
          Nothing ->
              die ("Unreadable data: " ++ show str)
          Just x ->
              return x

sendSizedThing :: (MonadIO m, Show a) => Handle -> a -> m ()
sendSizedThing h x = putSizedThing h (show x)

getSizedThing :: MonadIO m => Handle -> m String
getSizedThing h
 = do sizeStr <- liftIO $ hGetLine h
      case maybeRead sizeStr of
          Nothing ->
              die ("Bad size: " ++ show sizeStr)
          Just size ->
              do bs <- liftIO $ BS.hGet h size
                 line <- liftIO $ hGetLine h
                 if null line
                     then return $ BS.unpack bs
                     else die ("Stuff after data: " ++ show line)

putSizedThing :: MonadIO m => Handle -> String -> m ()
putSizedThing h str = do liftIO $ hPutStrLn h $ show $ length str
                         liftIO $ hPutStrLn h str

readFromFile :: (MonadIO m, Read a) => FilePath -> m a
readFromFile fp = do xs <- readBinaryFile fp
                     case maybeRead xs of
                         Nothing ->
                             die "Couldn't read from file"
                         Just x ->
                             return x

writeToFile :: (MonadIO m, Show a) => FilePath -> a -> m ()
writeToFile fp x = writeBinaryFile fp (show x)

getNumericDirectoryContents :: FilePath -> IO [Integer]
getNumericDirectoryContents fp = do xs <- getDirectoryContents fp
                                    f xs
    where f [] = return []
          f ("." : xs) = f xs
          f (".." : xs) = f xs
          f (x : xs) = case maybeRead x of
                       Nothing ->
                           die ("Bad directory entry: " ++ show x)
                       Just n ->
                           liftM (n :) (f xs)

getInterestingDirectoryContents :: FilePath -> IO [FilePath]
getInterestingDirectoryContents fp = do xs <- getDirectoryContents fp
                                        return $ filter interesting xs
    where interesting "."  = False
          interesting ".." = False
          interesting _    = True

ignoreDoesNotExist :: IO () -> IO ()
ignoreDoesNotExist io = io `catch` \e -> unless (isDoesNotExistError e)
                                                (throwIO e)

getTOD :: MonadIO m => m TimeOfDay
getTOD = do t <- liftIO $ getCurrentTime
            return $ timeToTimeOfDay $ utctDayTime t

mkTime :: Int -> Int -> TimeOfDay
mkTime hour mins = TimeOfDay {
                       todHour = hour,
                       todMin = mins,
                       todSec = 0
                   }

data UserInfo = UserInfo {
                    ui_password :: String,
                    ui_buildTime :: BuildTime
                }

data BuildTime = Timed TimeOfDay
               | Continuous

mkUserInfo :: String -> BuildTime -> UserInfo
mkUserInfo pass bt = UserInfo {
                          ui_password = pass,
                          ui_buildTime = bt
                      }

