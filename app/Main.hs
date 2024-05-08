{-# LANGUAGE OverloadedStrings #-}

import qualified Web.Scotty as WS
import Control.Monad.IO.Class(liftIO)
import Network.HTTP.Types (ok200,badRequest400)
import User.Actions(createUser, isUniqueUser, deactivateUser, createConn, validateCreds)
import User.Types(User, UsrConn, IpUsrCreds)
import Post.Actions(createPost, getUsrPosts)
import Post.Types(Post)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Syskeys (getKeys)

main ::IO()
main = do
    syskeys <- getKeys
    WS.scotty 3000 $ do
        WS.get "/" $
            WS.text "EATSY!!!!!!"
-----------------------------------------------------------------------------------
---------------------------------USER ENDPOINTS------------------------------------
-----------------------------------------------------------------------------------
        WS.get "/uname/check" $ do
            maybeQ <- WS.queryParamMaybe "ip_username" :: WS.ActionM (Maybe T.Text)
            case maybeQ of 
                Just param -> do
                    (respStatus, respBody) <- liftIO $ isUniqueUser syskeys param
                    WS.liftIO $ print respBody
                    WS.status respStatus
                    WS.setHeader "Content-Type" "application/json"
                    WS.text $ TL.fromStrict respBody
                Nothing -> do
                    WS.status badRequest400
                    WS.text "no input"
        
        WS.post "/user/create" $ do
            usr <- WS.jsonData :: WS.ActionM User
            (respStatus, respBody) <- liftIO $ createUser syskeys usr
            WS.liftIO $ print respStatus
            WS.status respStatus
            WS.setHeader "Content-Type" "application/json"
            WS.raw respBody  
            
        
        WS.patch "/user/deactivate/username/:username" $ do
            maybeUname <- WS.pathParamMaybe "username" :: WS.ActionM (Maybe T.Text)
            case maybeUname of
                Just uname -> do 
                    (respStatus, respBody) <- liftIO $ deactivateUser syskeys uname
                    WS.liftIO $ print respStatus  
                    WS.status respStatus
                    WS.setHeader "Content-Type" "application/json"
                    WS.raw respBody
                Nothing -> do
                    WS.status badRequest400
                    WS.text "username input not provided"
        
        WS.post "/login" $ do
            usrCreds <- WS.jsonData :: WS.ActionM IpUsrCreds
            (respStatus, maybeRespBody) <- liftIO $ validateCreds syskeys usrCreds
            liftIO $ print respStatus
            case maybeRespBody of 
                Just respBody -> do
                    WS.liftIO $ print respBody
                    WS.status respStatus
                    WS.setHeader "Content-Type" "application/json"
                    WS.json respBody
                _ -> do
                    WS.status respStatus
                    WS.text "Not Authorised"

        WS.post "/user/connect" $ do
            usrsConn <- WS.jsonData :: WS.ActionM UsrConn
            (respStatus, respBody) <- liftIO $ createConn syskeys usrsConn 
            WS.liftIO $ print respStatus
            WS.status respStatus
            WS.setHeader "Content-Type" "application/json"
            WS.raw respBody

-----------------------------------------------------------------------------------
---------------------------------POST ENDPOINTS------------------------------------
-----------------------------------------------------------------------------------
        WS.post "/post/create" $ do
            post <- WS.jsonData :: WS.ActionM Post
            (respStatus, respBody) <- liftIO $ createPost syskeys post
            WS.liftIO $ print respStatus 
            WS.status respStatus
            WS.setHeader "Content-Type" "application/json"
            WS.raw respBody

        WS.get "/post/getall" $ do
            maybeUsr1 <- WS.queryParamMaybe "user1" :: WS.ActionM (Maybe Int)
            maybeUsr2 <- WS.queryParamMaybe "user2" :: WS.ActionM (Maybe Int)
            case (maybeUsr1,maybeUsr2) of 
                (Nothing, _) -> do
                    WS.status badRequest400
                    WS.text "user1 input not provided"              
                (_, Nothing) -> do
                    WS.status badRequest400
                    WS.text "user2 input not provided"
                (Just usr1, Just usr2) -> do
                    maybePosts <- liftIO $ getUsrPosts syskeys usr1 usr2
                    case maybePosts of 
                        Just posts -> do 
                            WS.status ok200
                            WS.json posts
                        Nothing -> do
                            WS.status badRequest400
                            WS.text "Not connected or posts could not be parsed" 
