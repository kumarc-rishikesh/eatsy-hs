{-# LANGUAGE OverloadedStrings #-}

module User.Actions (
  createUser,
  isUniqueUser,
  deactivateUser
) where

import Network.HTTP.Types (Status, mkStatus)
import Network.HTTP.Simple as NS
import System.Environment
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as TE
import Data.Text (Text, unpack)

import qualified User.Types as UT

createUser :: UT.User -> IO Status
createUser user = do
    neureloKey <- getEnv "NEURELO_KEY"
    neureloApi <- getEnv "NEURELO_ENDPOINT"
    let 
        method = NS.parseRequest_ $ "POST " <> neureloApi <> "/rest/APPUSER/__one?"        
        request' = setRequestBodyJSON user $ setRequestHeader "X-API-KEY" [B.pack neureloKey] method
    resp <- httpLBS request'
    pure $ getResponseStatus resp

isUniqueUser :: Text -> IO Status
isUniqueUser userName = do
    neureloKey <- getEnv "NEURELO_KEY"
    neureloApi <- getEnv "NEURELO_ENDPOINT"
    let
        method = NS.parseRequest_ $ "GET " <> neureloApi  <> "/custom/getUserCountUName"
        request = setRequestHeader "X-API-KEY" [ B.pack neureloKey] method
        queryItem = ( "ip_username", Just (TE.encodeUtf8 userName))
        request' = NS.setRequestQueryString [queryItem] request
    resp <- httpLBS request'
    case (decode $ getResponseBody resp :: Maybe UT.UsrCount) of
        Just (UT.UsrCount count) -> 
            case count of
                "0" -> pure $ mkStatus 200 "Unique"
                _   -> pure $ mkStatus 200 "Non-Unique"
        _ -> pure $ mkStatus 400 "Bad Request"

deactivateUser :: Text -> IO Status
deactivateUser userName = do
    neureloKey <- getEnv "NEURELO_KEY"
    neureloApi <- getEnv "NEURELO_ENDPOINT"
    let
        patchFilter = "/username/" <> userName
        method = NS.parseRequest_ $ "PATCH " <> neureloApi <> "/rest/APPUSER" <> unpack patchFilter
        reqBody = encode $ UT.UsrActive { UT.isActive = False }
        reqBody' = NS.setRequestBodyLBS reqBody method
        request' = setRequestHeaders [("Content-Type","application/json") , ("X-API-KEY", B.pack neureloKey )] reqBody'
    resp <- httpLBS request'
    pure $ getResponseStatus resp
