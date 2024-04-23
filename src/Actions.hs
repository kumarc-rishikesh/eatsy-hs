{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Actions (
  UserW,
  createUser
) where

import qualified ConvertTypes as CT
import Data.Aeson
import GHC.Generics
import Network.HTTP.Simple as NS
import System.Environment
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)

data User = User {
    user_id :: Int,
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
        user_id_ <- obj .: "user_id"
        username_ <- obj .: "username"
        user_email_ <- obj .: "user_email"
        user_pw_ <- obj .: "user_pw"
        dob_ <- CT.convertDate <$> obj .: "dob"
        country_ <- obj .: "country"
        return $ UserW $ User user_id_ username_ user_email_ user_pw_ dob_ country_


createUser :: UserW -> IO String 
createUser usr = do
    neureloKey <- getEnv "NEURELO_KEY"
    let 
        method = "POST https://us-east-2.aws.neurelo.com/rest/APPUSER/__one?"        
        request = setRequestBodyJSON ( unUserW usr) $ setRequestHeader "X-API-KEY" [B.pack neureloKey] method
    resp <- httpLBS request 
    pure $ show $ getResponseStatus resp
