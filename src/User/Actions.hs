{-# LANGUAGE OverloadedStrings #-}

module User.Actions (
  createUser,
  isUniqueUser,
  deactivateUser,
  createConn
) where

import Network.HTTP.Types (Status, mkStatus)
import qualified Network.HTTP.Simple as NS
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Text (Text, unpack)
import Data.Aeson
import Control.Exception (throwIO)


import qualified User.Types as UT
import qualified Syskeys as SK


createUser :: SK.SysKeys -> UT.User -> IO Status
createUser sk user = do
    let 
        method = NS.parseRequest_ $ "POST " <>  SK.neureloEndpoint sk  <> "/rest/APPUSER/__one"        
        request' = NS.setRequestBodyJSON user $ NS.setRequestHeader "X-API-KEY" [B.pack $ SK.neureloKey sk] method
    resp <- NS.httpLBS request'
    if NS.getResponseStatusCode resp == 201
        then do
            usrId <- getUsrIDFromResp $ NS.getResponseBody resp
            -- TODO : add logic to handle if user is created but not connected with the same user
            _ <- createConn sk (UT.UsrConn (UT.userId usrId) (UT.userId usrId))
            pure ()
        else 
            pure ()            
    pure $ NS.getResponseStatus resp
    where 
        getUsrIDFromResp :: BLC.ByteString -> IO UT.UsrId
        getUsrIDFromResp resp = case eitherDecode resp of
            Left err -> throwIO $ userError $ "Error decoding JSON: " ++ err
            Right usrId -> return usrId

isUniqueUser :: SK.SysKeys -> Text -> IO Status
isUniqueUser sk userName = do
    let
        method = NS.parseRequest_ $ "GET " <> SK.neureloEndpoint sk  <> "/custom/getUserCountUName"
        request = NS.setRequestHeader "X-API-KEY" [ B.pack $ SK.neureloKey sk] method
        queryItem = ( "ip_username", Just (TE.encodeUtf8 userName))
        request' = NS.setRequestQueryString [queryItem] request
    resp <- NS.httpLBS request'
    case (decode $ NS.getResponseBody resp :: Maybe UT.UsrCount) of
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
        request' = NS.setRequestHeaders [("Content-Type","application/json") , ("X-API-KEY", B.pack $ SK.neureloKey sk )] reqBody'
    resp <- NS.httpLBS request'
    pure $ NS.getResponseStatus resp

createConn :: SK.SysKeys -> UT.UsrConn -> IO Status
createConn sk usrsConn = do
    let 
        method = NS.parseRequest_ $ "POST " <> SK.neureloEndpoint sk <> "/rest/USER_REL/__one"
        request' = NS.setRequestBodyJSON usrsConn $ NS.setRequestHeader "X-API-KEY" [B.pack $ SK.neureloKey sk] method
    resp <- NS.httpLBS request'
    pure $ NS.getResponseStatus resp
