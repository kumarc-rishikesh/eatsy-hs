{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module User.Types (
  User(..),
  UsrCount(..),
  UsrActive(..),
  UsrConn(..)
) where

import qualified ConvertTypes as CT
import Data.Aeson
import GHC.Generics
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
    } deriving (Generic, Show)

instance ToJSON UsrCount

instance FromJSON UsrCount  where
    parseJSON = withObject "UsrCount" $ \v -> do
        dataArray <- v .: "data"
        case dataArray of
            [obj] -> do
                count <- obj .: "count"
                pure $ UsrCount count
            _ -> fail "Expected array with a single object"


newtype UsrActive = UsrActive
    { isActive :: Bool
    } deriving (Generic, Show)

instance ToJSON UsrActive where
    toJSON (UsrActive isActive') =
        object ["is_active" .= isActive']

data UsrConn = UsrConn {
    user1 :: Int,
    user2 :: Int
} deriving (Generic, Show)
instance ToJSON UsrConn where 
    toJSON (UsrConn usr1 usr2) = 
        object ["user1" .= usr1 , "APPUSER" .= object ["connect" .= object ["user_id" .= usr2 ]]]
    
instance FromJSON UsrConn
