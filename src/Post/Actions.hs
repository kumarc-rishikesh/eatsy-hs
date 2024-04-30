{-# Language OverloadedStrings #-}

module Post.Actions (
  createPost,
  getUsrPosts
) where

import Network.HTTP.Types (Status)
import qualified Network.HTTP.Simple as NS
import qualified Data.ByteString.Char8 as B
import User.Actions(getUserConnections)
import Data.Aeson.Types(parseMaybe)
import Data.Aeson
import qualified Post.Types as PT 
import qualified Syskeys as SK

createPost :: SK.SysKeys -> PT.Post -> IO Status
createPost sk post = do
    let 
        method = NS.parseRequest_ $ "POST " <> SK.neureloEndpoint sk <> "/rest/POSTS/__one"
        request' = NS.setRequestBodyJSON post $ NS.setRequestHeader "X-API-KEY" [B.pack $ SK.neureloKey sk] method
    resp <- NS.httpLBS request'
    pure $ NS.getResponseStatus resp

getUsrPosts :: SK.SysKeys -> Int -> Int -> IO (Maybe [PT.OPPost])
getUsrPosts sk usr1 usr2 = do
    usrs <- getUserConnections sk usr1
    case usrs of 
        Nothing -> pure Nothing 
        Just usr2s -> do 
            if usr2 `elem` usr2s
            then do 
                let
                    method =  NS.parseRequest_ $ "GET " <> SK.neureloEndpoint sk  <> "/rest/POSTS"
                    request = NS.setRequestHeader "X-API-KEY" [B.pack $ SK.neureloKey sk] method
                    queryParamArr = [("filter", Just $ B.pack $ "{\"appuser_user_id\":{\"equals\":" <> show usr2 <> "}}")]
                    request' = NS.setRequestQueryString queryParamArr request
                resp <- NS.httpLBS request'
                let 
                    parsedResp = decode $ NS.getResponseBody resp :: Maybe Object
                    usrPosts = parsedResp >>= parseMaybe (.: "data") :: Maybe [PT.OPPost]
                case usrPosts of 
                    Just usrPostsList -> 
                        pure $ Just usrPostsList
                    Nothing -> do
                        putStrLn "Failed to get posts"
                        pure Nothing
            else do 
                putStrLn "User does not have access"
                pure Nothing