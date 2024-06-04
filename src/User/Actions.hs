{-# LANGUAGE OverloadedStrings #-}

module User.Actions (
  createUser,
  isUniqueUser,
  deactivateUser,
  createConn,
  getUserConnections,
  getUserId,
  validateCreds
) where

import Network.HTTP.Types (Status, ok200, badRequest400, unauthorized401)
import qualified Network.HTTP.Simple as NS
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as BLC
import Control.Monad (when)
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types(parseMaybe)
import Control.Exception (throwIO)


import qualified User.Types as UT
import qualified Utils as U


createUser :: U.SysKeys -> UT.User -> IO (Status, BLC.ByteString)
createUser sk user = do
    let 
        method = NS.parseRequest_ $ "POST " <>  U.neureloEndpoint sk  <> "/rest/APPUSER/__one"        
        request' = NS.setRequestBodyJSON user $ NS.setRequestHeader "X-API-KEY" [B.pack $ U.neureloKey sk] method
    resp <- NS.httpLBS request' 
    when (NS.getResponseStatusCode resp ==201) $ do
        usrId <- getUsrIDFromResp $ NS.getResponseBody resp
        _ <- createConn sk (UT.UsrConn (UT.userId usrId) (UT.userId usrId))
        pure()
    pure (NS.getResponseStatus resp, NS.getResponseBody resp)
    where 
        getUsrIDFromResp :: BLC.ByteString -> IO UT.UsrId
        getUsrIDFromResp resp = case eitherDecode resp of
            Left err -> throwIO $ userError $ "Error decoding JSON: " ++ err
            Right usrId -> return usrId

isUniqueUser :: U.SysKeys -> Text -> IO (Status, Text)
isUniqueUser sk userName = do
    let
        method = NS.parseRequest_ $ "GET " <> U.neureloEndpoint sk  <> "/custom/getUserCountUName"
        request = NS.setRequestHeader "X-API-KEY" [ B.pack $ U.neureloKey sk] method
        queryItem = ( "ip_username", Just (TE.encodeUtf8 userName))
        request' = NS.setRequestQueryString [queryItem] request
    resp <- NS.httpLBS request'
    case (decode $ NS.getResponseBody resp :: Maybe UT.UsrCount) of
        Just (UT.UsrCount count) -> 
            case count of
                "0" -> pure (ok200, "Unique")
                _   -> pure (ok200, "Non-Unique")
        _ -> pure (badRequest400, "Bad Request")

getUserId :: U.SysKeys -> Text -> IO (Maybe Int)
getUserId sk userEmail = do
    let
        method = NS.parseRequest_ $ "GET " <> U.neureloEndpoint sk <> "/custom/getUserIDEmail"
        request = NS.setRequestHeader "X-API-KEY" [ B.pack $ U.neureloKey sk ] method
        queryItem = ( "ip_user_email", Just (TE.encodeUtf8 userEmail))
        request' = NS.setRequestQueryString [queryItem] request
    resp <- NS.httpLBS request'
    case (decode $ NS.getResponseBody resp :: Maybe UT.UsrId) of
        Just usrId ->
            pure (Just $ UT.userId usrId)
        _ -> pure Nothing

validateCreds :: U.SysKeys -> UT.IpUsrCreds -> IO (Status, Maybe UT.UsrCreds)
validateCreds sk ipUsrCreds = do 
    userId <- getUserId sk (UT.usrEmailIp ipUsrCreds)
    case userId of 
        Just uid -> do
            let 
                method = NS.parseRequest_ $ "GET " <>U.neureloEndpoint sk <> "/rest/APPUSER/" <> show uid 
                request' = NS.setRequestHeader "X-API-KEY" [ B.pack $ U.neureloKey sk ] method
            resp <- NS.httpLBS request'
            case (decode $ NS.getResponseBody resp :: Maybe UT.UsrCreds) of
                Just cred -> do
                    print cred
                    if UT.userPwCred cred == UT.usrPwIp ipUsrCreds
                    then
                        pure (ok200, Just cred)
                    else 
                        pure (unauthorized401, Nothing)
                _ -> pure (badRequest400,Nothing)
        _ -> pure (badRequest400, Nothing)

deactivateConn :: U.SysKeys -> Int -> IO (Int, Status, BLC.ByteString)
deactivateConn sk userId = do 
    let
        method = NS.parseRequest_ $ "PATCH " <> U.neureloEndpoint sk <> "/rest/USER_REL/"
        reqBody = encode $ UT.ActiveStatus { UT.isActive = False }
        reqBody' = NS.setRequestBodyLBS reqBody method
        qFilters = "{\"OR\":[{\"user1\":{\"equals\":" <> B.pack ( show userId) <> " }},{\"user2\":{\"equals\":" <> B.pack ( show userId) <> " }}]}"
        queryParamArr = [("filter", Just qFilters)]
        request = NS.setRequestQueryString queryParamArr reqBody'
        request' = NS.setRequestHeader "X-API-KEY" [ B.pack $ U.neureloKey sk ] request
    resp <- NS.httpLBS request'
    pure (NS.getResponseStatusCode resp, NS.getResponseStatus resp, NS.getResponseBody resp)

deactivateUser :: U.SysKeys -> Int -> IO (Status, BLC.ByteString)
deactivateUser sk userId = do
    let
        method = NS.parseRequest_ $ "PATCH " <> U.neureloEndpoint sk <> "/rest/APPUSER/" <> show userId
        reqBody = encode $ UT.ActiveStatus { UT.isActive = False }
        reqBody' = NS.setRequestBodyLBS reqBody method
        request' = NS.setRequestHeaders [("Content-Type","application/json") , ("X-API-KEY", B.pack $ U.neureloKey sk )] reqBody'
    resp <- NS.httpLBS request'
    case NS.getResponseStatusCode resp of
        200 ->  do
            (dConnCode, dConnStatus, dConnBody) <- deactivateConn sk userId
            case dConnCode of
                200 -> pure(ok200, "{ "<> NS.getResponseBody resp <> "," <> dConnBody <> " }")
                _ -> pure (dConnStatus, dConnBody)        
        _ -> pure (NS.getResponseStatus resp, NS.getResponseBody resp)

createConn :: U.SysKeys -> UT.UsrConn -> IO (Status, BLC.ByteString)
createConn sk usrsConn = do
    let 
        method = NS.parseRequest_ $ "POST " <> U.neureloEndpoint sk <> "/rest/USER_REL/__one"
        request' = NS.setRequestBodyJSON usrsConn $ NS.setRequestHeader "X-API-KEY" [B.pack $ U.neureloKey sk] method
    resp <- NS.httpLBS request'
    pure (NS.getResponseStatus resp, NS.getResponseBody resp)

getUserConnections :: U.SysKeys -> Int -> IO (Maybe [Int])
getUserConnections sk user1 = do
    let 
        method = NS.parseRequest_ $ "GET " <> U.neureloEndpoint sk  <> "/rest/USER_REL"
        request = NS.setRequestHeader "X-API-KEY" [B.pack $ U.neureloKey sk] method
        qFilters = "{\"AND\":[{\"user1\":{\"equals\":" <> B.pack (show user1) <> "}},{\"is_active\":{\"equals\": true}}]}" :: B.ByteString
        queryParamArr = [("select", Just "{\"user2\":true}"), 
            ("filter", Just qFilters )]
        request' = NS.setRequestQueryString queryParamArr request
    resp <- NS.httpLBS request'
    let 
        parsedResp = decode $ NS.getResponseBody resp :: Maybe Object
        userConns = parsedResp >>= parseMaybe (.: "data") :: Maybe [UT.UsrConn2] 
        extractUsr :: [UT.UsrConn2] -> [Int]
        extractUsr = map UT.usr
    case userConns of
        Just userConnList -> do
            pure $ Just $ extractUsr userConnList
        Nothing -> do 
            putStrLn "Failed to parse JSON or extract user connections"
            pure Nothing
            
