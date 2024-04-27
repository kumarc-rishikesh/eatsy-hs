{-# LANGUAGE OverloadedStrings #-}

module User.Actions (
  createUser,
  isUniqueUser,
  deactivateUser,
  createConn
) where

import Network.HTTP.Types (Status, mkStatus)
import Network.HTTP.Simple as NS
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as TE
import Data.Text (Text, unpack)

import qualified User.Types as UT
import qualified Syskeys as SK

createUser :: SK.SysKeys -> UT.User -> IO Status
createUser sk user = do
    let 
        method = NS.parseRequest_ $ "POST " <>  SK.neureloEndpoint sk  <> "/rest/APPUSER/__one"        
        request' = setRequestBodyJSON user $ setRequestHeader "X-API-KEY" [B.pack $ SK.neureloKey sk] method
    resp <- httpLBS request'
    pure $ getResponseStatus resp

isUniqueUser :: SK.SysKeys -> Text -> IO Status
isUniqueUser sk userName = do
    let
        method = NS.parseRequest_ $ "GET " <> SK.neureloEndpoint sk  <> "/custom/getUserCountUName"
        request = setRequestHeader "X-API-KEY" [ B.pack $ SK.neureloKey sk] method
        queryItem = ( "ip_username", Just (TE.encodeUtf8 userName))
        request' = NS.setRequestQueryString [queryItem] request
    resp <- httpLBS request'
    case (decode $ getResponseBody resp :: Maybe UT.UsrCount) of
        Just (UT.UsrCount count) -> 
            case count of
                "0" -> pure $ mkStatus 200 "Unique"
                _   -> pure $ mkStatus 200 "Non-Unique"
        _ -> pure $ mkStatus 400 "Bad Request"

deactivateUser :: SK.SysKeys -> Text -> IO Status
deactivateUser sk userName = do
    let
        patchFilter = "/username/" <> userName
        method = NS.parseRequest_ $ "PATCH " <> SK.neureloEndpoint sk <> "/rest/APPUSER" <> unpack patchFilter
        reqBody = encode $ UT.UsrActive { UT.isActive = False }
        reqBody' = NS.setRequestBodyLBS reqBody method
        request' = setRequestHeaders [("Content-Type","application/json") , ("X-API-KEY", B.pack $ SK.neureloKey sk )] reqBody'
    resp <- httpLBS request'
    pure $ getResponseStatus resp

createConn :: SK.SysKeys -> UT.UsrConn -> IO Status
createConn sk usrsConn = do
    let 
        method = NS.parseRequest_ $ "POST " <> SK.neureloEndpoint sk <> "/rest/USER_REL/__one"
        request' = setRequestBodyJSON usrsConn $ setRequestHeader "X-API-KEY" [B.pack $ SK.neureloKey sk] method
    resp <- httpLBS request'
    pure $ getResponseStatus resp
