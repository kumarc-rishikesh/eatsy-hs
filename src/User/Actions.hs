{-# LANGUAGE OverloadedStrings #-}

module User.Actions (
  createUser,
  isUniqueUser,
  deactivateUser,
  createConn,
  getUserConnections
) where

import Network.HTTP.Types (Status, ok200, badRequest400)
import qualified Network.HTTP.Simple as NS
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as BLC
import Control.Monad (when)
import Data.Text (Text, unpack)
import Data.Aeson
import Data.Aeson.Types(parseMaybe)
import Control.Exception (throwIO)


import qualified User.Types as UT
import qualified Syskeys as SK


createUser :: SK.SysKeys -> UT.User -> IO (Status, BLC.ByteString)
createUser sk user = do
    let 
        method = NS.parseRequest_ $ "POST " <>  SK.neureloEndpoint sk  <> "/rest/APPUSER/__one"        
        request' = NS.setRequestBodyJSON user $ NS.setRequestHeader "X-API-KEY" [B.pack $ SK.neureloKey sk] method
    resp <- NS.httpLBS request' 
    when (NS.getResponseStatusCode resp ==201) $ do
        usrId <- getUsrIDFromResp $ NS.getResponseBody resp
        -- TODO : add logic to handle if user is created but not connected with the same user
        _ <- createConn sk (UT.UsrConn (UT.userId usrId) (UT.userId usrId))
        pure()
    pure (NS.getResponseStatus resp, NS.getResponseBody resp)
    where 
        getUsrIDFromResp :: BLC.ByteString -> IO UT.UsrId
        getUsrIDFromResp resp = case eitherDecode resp of
            Left err -> throwIO $ userError $ "Error decoding JSON: " ++ err
            Right usrId -> return usrId

isUniqueUser :: SK.SysKeys -> Text -> IO (Status, Text)
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
                "0" -> pure (ok200, "Unique")
                _   -> pure (ok200, "Non-Unique")
        _ -> pure (badRequest400, "Bad Request")

deactivateUser :: SK.SysKeys -> Text -> IO (Status, BLC.ByteString)
deactivateUser sk userName = do
    let
        patchFilter = "/username/" <> userName
        method = NS.parseRequest_ $ "PATCH " <> SK.neureloEndpoint sk <> "/rest/APPUSER" <> unpack patchFilter
        reqBody = encode $ UT.UsrActive { UT.isActive = False }
        reqBody' = NS.setRequestBodyLBS reqBody method
        request' = NS.setRequestHeaders [("Content-Type","application/json") , ("X-API-KEY", B.pack $ SK.neureloKey sk )] reqBody'
    resp <- NS.httpLBS request'
    pure (NS.getResponseStatus resp, NS.getResponseBody resp)

createConn :: SK.SysKeys -> UT.UsrConn -> IO (Status, BLC.ByteString)
createConn sk usrsConn = do
    let 
        method = NS.parseRequest_ $ "POST " <> SK.neureloEndpoint sk <> "/rest/USER_REL/__one"
        request' = NS.setRequestBodyJSON usrsConn $ NS.setRequestHeader "X-API-KEY" [B.pack $ SK.neureloKey sk] method
    resp <- NS.httpLBS request'
    pure (NS.getResponseStatus resp, NS.getResponseBody resp)

getUserConnections :: SK.SysKeys -> Int -> IO (Maybe [Int])
getUserConnections sk user1 = do
    let 
        method = NS.parseRequest_ $ "GET " <> SK.neureloEndpoint sk  <> "/rest/USER_REL"
        request = NS.setRequestHeader "X-API-KEY" [B.pack $ SK.neureloKey sk] method
        queryParamArr = [("select", Just "{\"user2\":true}"), 
            ("filter", Just $ B.pack $ "{\"user1\":{\"equals\":" <> show user1 <> "}}")]
        request' = NS.setRequestQueryString queryParamArr request
    resp <- NS.httpLBS request'
    let 
        parsedResp = decode $ NS.getResponseBody resp :: Maybe Object
        userConns = parsedResp >>= parseMaybe (.: "data") :: Maybe [UT.UserConn] 
        extractUsr :: [UT.UserConn] -> [Int]
        extractUsr = map UT.usr
    case userConns of
        Just userConnList -> do
            pure $ Just $ extractUsr userConnList
        Nothing -> do 
            putStrLn "Failed to parse JSON or extract user connections"
            pure Nothing
            
