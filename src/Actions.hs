{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Actions (
  User,
  createUser,
  isUniqueUser
) where

import qualified ConvertTypes as CT
import Data.Aeson
import GHC.Generics
import Network.HTTP.Types (Status, mkStatus)
import Network.HTTP.Simple as NS
import System.Environment
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as TE
import Data.Text (Text, strip)

data User = User {
    username :: Text,
    user_email :: Text,
    user_pw :: Text,
    dob :: Text,
    country :: Text
} deriving (Generic, Show)
instance ToJSON User

instance FromJSON User where
    parseJSON = withObject "UserW" $ \obj -> do
        username_ <- strip <$> obj .: "username"
        user_email_ <- strip <$> obj .: "user_email"
        user_pw_ <- strip <$> obj .: "user_pw"
        dob_ <- CT.convertDate . strip <$> obj .: "dob"
        country_ <- strip <$> obj .: "country"
        return $ User username_ user_email_ user_pw_ dob_ country_

newtype UsrCount = UsrCount
    { userCount :: Text
    } deriving (Show, Generic)

instance FromJSON UsrCount  where
    parseJSON = withObject "UsrCount" $ \v -> do
        dataArray <- v .: "data"
        case dataArray of
            [obj] -> do
                count <- obj .: "count"
                pure $ UsrCount count
            _ -> fail "Expected array with a single object"

createUser :: User -> IO Status
createUser user = do
    neureloKey <- getEnv "NEURELO_KEY"
    neureloApi <- getEnv "NEURELO_ENDPOINT"
    let 
        method = NS.parseRequest_ $ "POST " <> neureloApi <> "/rest/APPUSER/__one?"        
        request = setRequestBodyJSON user $ setRequestHeader "X-API-KEY" [B.pack neureloKey] method
    resp <- httpLBS request 
    pure $ getResponseStatus resp

isUniqueUser :: Text -> IO Status
isUniqueUser userName = do
    neureloKey <- getEnv "NEURELO_KEY"
    neureloApi <- getEnv "NEURELO_ENDPOINT"
    let
        method = NS.parseRequest_ $ "GET " <> neureloApi  <> "/custom/getUserCountUName"
        request = setRequestHeader "X-API-KEY" [ B.pack neureloKey] method
        queryItem = ( "ip_username", Just (TE.encodeUtf8 userName))
        request_ = NS.setRequestQueryString [queryItem] request
    resp <- httpLBS request_
    case (decode $ getResponseBody resp :: Maybe UsrCount) of
        Just (UsrCount count) -> 
            case count of
                "0" -> pure $ mkStatus 200 "Unique"
                _   -> pure $ mkStatus 200 "Non-Unique"
        _ -> pure $ mkStatus 400 "Bad Request"
