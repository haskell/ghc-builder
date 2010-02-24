
module Utils (User, Pass, port, Verbosity (..), Result(..),
              die, maybeRead,
              readBinaryFile, maybeReadBinaryFile,
              writeBinaryFile, maybeWriteBinaryFile,
              readFromFile, maybeReadFromFile,
              writeToFile,
              getMaybeSizedThing, putMaybeSizedThing,
              readSizedThing, sendSizedThing,
              getSortedNumericDirectoryContents,
              onDoesNotExist, onEndOfFile, ignoreDoesNotExist,
              Instructions(..),
              getTOD, mkTime, UserInfo(..), BuildTime(..), mkUserInfo
             ) where

import BuildStep
import Handlelike

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Time.Clock
import Data.Time.LocalTime
import Prelude hiding (catch)
import System.Directory
import System.Exit
import System.IO
import System.IO.Error hiding (catch)

type User = String
type Pass = String

port :: Int
port = 4938 -- A random number

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

maybeReadBinaryFile :: MonadIO m => FilePath -> m (Maybe String)
maybeReadBinaryFile fp
 = do mh <- liftIO (liftM Just (openBinaryFile fp ReadMode)
                    `onDoesNotExist` return Nothing)
      case mh of
          Just h -> liftIO $ liftM Just $ hGetContents h
          Nothing -> return Nothing

writeBinaryFile :: MonadIO m => FilePath -> String -> m ()
writeBinaryFile fp str
    = liftIO $ withBinaryFile fp WriteMode (\h -> hPutStr h str)

maybeWriteBinaryFile :: MonadIO m => FilePath -> Maybe String -> m ()
maybeWriteBinaryFile _  Nothing    = return () -- Could delete it if it exists?
maybeWriteBinaryFile fp (Just str) = writeBinaryFile fp str

readSizedThing :: (MonadIO m, HandlelikeM m, Read a) => m a
readSizedThing
 = do str <- getSizedThing
      case maybeRead str of
          Nothing ->
              die ("Unreadable data: " ++ show str)
          Just x ->
              return x

sendSizedThing :: (HandlelikeM m, Show a) => a -> m ()
sendSizedThing x = putSizedThing (show x)

getSizedThing :: (MonadIO m, HandlelikeM m) => m String
getSizedThing
 = do sizeStr <- hlGetLine
      case maybeRead sizeStr of
          Nothing ->
              die ("Bad size: " ++ show sizeStr)
          Just size ->
              do s <- hlGet size
                 line <- hlGetLine
                 if null line
                     then return s
                     else die ("Stuff after data: " ++ show line)

getMaybeSizedThing :: (MonadIO m, HandlelikeM m) => m (Maybe String)
getMaybeSizedThing
 = do sizeStr <- hlGetLine
      if sizeStr == "NONE"
          then return Nothing
          else case maybeRead sizeStr of
               Nothing ->
                   die ("Bad size: " ++ show sizeStr)
               Just size ->
                   do s <- hlGet size
                      line <- hlGetLine
                      if null line
                          then return (Just s)
                          else die ("Stuff after data: " ++ show line)

putSizedThing :: HandlelikeM m => String -> m ()
putSizedThing str = do hlPutStrLn $ show $ length str
                       hlPutStrLn str

putNoSizedThing :: HandlelikeM m => m ()
putNoSizedThing = hlPutStrLn "NONE"

putMaybeSizedThing :: HandlelikeM m => Maybe String -> m ()
putMaybeSizedThing Nothing = putNoSizedThing
putMaybeSizedThing (Just str) = putSizedThing str

readFromFile :: (MonadIO m, Read a) => FilePath -> m a
readFromFile fp = do xs <- readBinaryFile fp
                     case maybeRead xs of
                         Nothing ->
                             die "Couldn't read from file"
                         Just x ->
                             return x

maybeReadFromFile :: (MonadIO m, Read a) => FilePath -> m (Maybe a)
maybeReadFromFile fp
 = do mxs <- maybeReadBinaryFile fp
      case mxs of
          Just xs ->
              case maybeRead xs of
                  Nothing ->
                      return Nothing
                  Just x ->
                      return (Just x)
          Nothing ->
              return Nothing

writeToFile :: (MonadIO m, Show a) => FilePath -> a -> m ()
writeToFile fp x = writeBinaryFile fp (show x)

getSortedNumericDirectoryContents :: FilePath -> IO [Integer]
getSortedNumericDirectoryContents fp
    = liftM sort $ getNumericDirectoryContents fp

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

ignoreDoesNotExist :: IO () -> IO ()
ignoreDoesNotExist io = io `catch` \e -> unless (isDoesNotExistError e)
                                                (throwIO e)

onDoesNotExist :: IO a -> IO a -> IO a
onDoesNotExist io io' = io `catch` \e -> if isDoesNotExistError e
                                         then io'
                                         else throwIO e

onEndOfFile :: IO a -> IO a -> IO a
onEndOfFile io io' = io `catch` \e -> if isEOFError e
                                      then io'
                                      else throwIO e

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
                    ui_buildTime :: BuildTime,
                    ui_buildInstructions :: [BuildStep]
                }

data Instructions = Idle
                  | StartBuild BuildTime
    deriving (Show, Read)

data BuildTime = Timed TimeOfDay
               | Continuous
    deriving (Show, Read)

mkUserInfo :: String -> BuildTime -> [BuildStep] -> UserInfo
mkUserInfo pass bt bis
    = UserInfo {
          ui_password = pass,
          ui_buildTime = bt,
          ui_buildInstructions = bis
      }

