{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Actions (
  UserW,
  createUser
) where

import qualified ConvertTypes as CT
import Data.Aeson
import GHC.Generics
import Network.HTTP.Types (Status, mkStatus)
import Network.HTTP.Simple as NS
import System.Environment
import qualified Data.ByteString.Char8 as B
import Data.Text (Text, strip)

data User = User {
    username :: Text,
    user_email :: Text,
    user_pw :: Text,
    dob :: Text,
    country :: Text
} deriving (Generic, Show)
instance ToJSON User

newtype UserW = UserW { unUserW :: User }
instance FromJSON UserW where
    parseJSON = withObject "UserW" $ \obj -> do
        username_ <- strip <$> obj .: "username"
        user_email_ <- strip <$> obj .: "user_email"
        user_pw_ <- strip <$> obj .: "user_pw"
        dob_ <- CT.convertDate . strip <$> obj .: "dob"
        country_ <- strip <$> obj .: "country"
        return $ UserW $ User username_ user_email_ user_pw_ dob_ country_

createUser :: UserW -> IO Status
createUser (UserW user) = do
        neureloKey <- getEnv "NEURELO_KEY"
        neureloApi <- getEnv "NEURELO_ENDPOINT"
        let 
            method = NS.parseRequest_ $ "POST " <> neureloApi <> "/rest/APPUSER/__one?"        
            request = setRequestBodyJSON user $ setRequestHeader "X-API-KEY" [B.pack neureloKey] method
        resp <- httpLBS request 
        pure $ getResponseStatus resp
