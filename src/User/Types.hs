{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module User.Types (
  User(..),
  UsrCount(..),
  ActiveStatus(..),
  UsrConn(..),
  UsrId(..),
  UsrConn2(..),
  UsrCreds(..),
  IpUsrCreds(..)
) where

import qualified Utils as U
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
    parseJSON = withObject "User" $ \obj -> do
        username_ <- strip <$> obj .: "username"
        user_email_ <- strip <$> obj .: "user_email"
        user_pw_ <- strip <$> obj .: "user_pw"
        dob_ <- U.convertDate . strip <$> obj .: "dob"
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


newtype ActiveStatus = ActiveStatus
    { isActive :: Bool
    } deriving (Generic, Show)

instance ToJSON ActiveStatus where
    toJSON (ActiveStatus isActive') =
        object ["is_active" .= isActive']

data UsrConn = UsrConn {
    user1 :: Int,
    user2 :: Int
} deriving (Generic, Show)
instance ToJSON UsrConn where 
    toJSON (UsrConn usr1 usr2) = 
        object ["user1" .= usr1 , "APPUSER" .= object ["connect" .= object ["user_id" .= usr2 ]]]
instance FromJSON UsrConn

newtype UsrConn2 = UsrConn2 {
    usr :: Int
} deriving(Generic,Show)
instance FromJSON UsrConn2 where
    parseJSON = withObject "UsrConn2" $ \obj -> do
        usr_ <- obj .: "user2"
        return $ UsrConn2 usr_

newtype UsrId = UsrId {
    userId :: Int
} deriving (Generic, Show)
instance FromJSON UsrId where 
    parseJSON = withObject "UsrId" $ \v -> do 
        dataObj <- v .: "data"
        case dataObj of
            [obj]-> do
                usrId_ <- obj .: "user_id"
                pure $ UsrId usrId_  
            _ -> fail "Unexpected user_id response"

data IpUsrCreds = IpUsrCreds{
    usrEmailIp :: Text,
    usrPwIp :: Text
} deriving (Generic,Show)
instance FromJSON IpUsrCreds where
    parseJSON = withObject "IpUsrCreds" $ \obj -> do
        usrEmailIp_ <- obj .: "user_email"
        usrPwIp_ <- obj .: "user_pw"
        return $ IpUsrCreds usrEmailIp_  usrPwIp_

data UsrCreds = UsrCreds{
    userIdCred :: Int,
    userEmailCred :: Text,
    userPwCred :: Text,
    userNameCred :: Text
} deriving (Generic, Show)
instance FromJSON UsrCreds where 
    parseJSON = withObject "UsrCreds" $ \v -> do
        dataObj <- v .: "data"
        userIdCred_ <- dataObj .: "user_id"
        userEmailCred_ <- dataObj .: "user_email"
        userPwCred_ <- dataObj .: "user_pw"
        userNameCred_ <- dataObj .: "username"
        return $ UsrCreds userIdCred_ userEmailCred_ userPwCred_ userNameCred_
instance ToJSON UsrCreds where 
    toJSON (UsrCreds userIdCred_ userEmailCred_ _ userNameCred_ ) = 
        object [
            "email" .= userEmailCred_,
            "userId".= userIdCred_,
            "name" .= userNameCred_
        ]
