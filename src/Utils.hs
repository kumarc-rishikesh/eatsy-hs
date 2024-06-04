module Utils where

import System.Environment
import Data.Time.Format
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T


data SysKeys = SysKeys
    { neureloKey :: String,
      neureloEndpoint :: String
    } deriving (Show)

getKeys :: IO SysKeys
getKeys = do 
    nKey <- getEnv "NEURELO_KEY"
    nApi <- getEnv "NEURELO_ENDPOINT"
    let syskeys = SysKeys nKey nApi
    pure syskeys 

convertDate :: Text -> Text
convertDate inputDate = do
    let parsedDate =  parseTimeOrError True defaultTimeLocale "%Y-%m-%d" (T.unpack inputDate) :: UTCTime
    T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%qZ" parsedDate
