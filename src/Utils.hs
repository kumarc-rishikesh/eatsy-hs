{-# LANGUAGE OverloadedStrings #-}
module Utils where

import System.Environment
import Data.Time.Format
import Data.Time.Clock
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import qualified Web.JWT as JWT
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Aeson


data SysKeys = SysKeys
    { neureloKey ::  String,
      neureloEndpoint :: String
    } deriving (Show)

getKeys :: IO SysKeys
getKeys = do 
    nKey <- getEnv "NEURELO_KEY"
    nApi <- getEnv "NEURELO_ENDPOINT"
    pure $ SysKeys nKey nApi
    
convertDate :: Text -> Text
convertDate inputDate = do
    let parsedDate =  parseTimeOrError True defaultTimeLocale "%Y-%m-%d" (T.unpack inputDate) :: UTCTime
    T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%qZ" parsedDate

decodeToken :: Text -> IO (Maybe Int)
decodeToken token = do
    jwtKey <- getEnv "JWT_KEY"
    let mJWT = JWT.claims <$> JWT.decodeAndVerifySignature ( JWT.toVerify . JWT.hmacSecret . pack $ jwtKey) token
    case mJWT of 
        Nothing -> pure Nothing 
        Just jwt -> do
            let res = Map.lookup "user_id" $ JWT.unClaimsMap $ JWT.unregisteredClaims jwt
            case res of 
                Nothing -> pure Nothing 
                Just (Number user_id) -> pure $ Just $ fromMaybe (0::Int) $ toBoundedInteger user_id
                _ -> pure Nothing

createToken :: Int -> IO Text
createToken userId = do
    jwtKey <- getEnv "JWT_KEY"
    let 
        key = JWT.hmacSecret . pack $ jwtKey
        cs = mempty {
        JWT.iss = JWT.stringOrURI . pack $ jwtKey
        , JWT.unregisteredClaims = JWT.ClaimsMap $ Map.fromList [(pack "user_id" , Number $ fromIntegral userId )]
        }
    pure $ JWT.encodeSigned key mempty cs
