{-# LANGUAGE OverloadedStrings #-}

import qualified Web.Scotty as WS
import Control.Monad.IO.Class(liftIO)
import User.Actions(createUser, isUniqueUser, deactivateUser, createConn)
import User.Types(User, UsrConn)
import Post.Actions(createPost, getUsrPosts)
import Post.Types(Post)
import qualified Data.Text as T
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
                    resp <- liftIO $ isUniqueUser syskeys param                               
                    WS.liftIO $ print resp  
                    WS.status resp
                Nothing -> liftIO $ putStrLn "no input"
        
        WS.post "/user/create" $ do
            usr <- WS.jsonData :: WS.ActionM User
            resp <- liftIO $ createUser syskeys usr
            WS.liftIO $ print resp  
            WS.status resp
        
        WS.patch "/user/deactivate/username/:username" $ do
            maybeUname <- WS.pathParamMaybe "username" :: WS.ActionM (Maybe T.Text)
            case maybeUname of
                Just uname -> do 
                    resp <- liftIO $ deactivateUser syskeys uname
                    WS.liftIO $ print resp  
                    WS.status resp
                Nothing -> liftIO $ putStrLn "no input"

        WS.post "/user/connect" $ do
            usrsConn <- WS.jsonData :: WS.ActionM UsrConn
            resp <- liftIO $ createConn syskeys usrsConn 
            WS.liftIO $ print resp  
            WS.status resp

-----------------------------------------------------------------------------------
---------------------------------POST ENDPOINTS------------------------------------
-----------------------------------------------------------------------------------
        WS.post "/post/create" $ do
            post <- WS.jsonData :: WS.ActionM Post
            resp <- liftIO $ createPost syskeys post
            WS.liftIO $ print resp 
            WS.status resp

        WS.get "/post/getall" $ do
            maybeUsr1 <- WS.queryParamMaybe "user1" :: WS.ActionM (Maybe Int)
            maybeUsr2 <- WS.queryParamMaybe "user2" :: WS.ActionM (Maybe Int)
            case (maybeUsr1,maybeUsr2) of 
                (Nothing, _) -> liftIO $ putStrLn "user1 input not provided"
                (_, Nothing) -> liftIO $ putStrLn "user2 input not provided"
                (Just usr1, Just usr2) -> do
                    maybePosts <- liftIO $ getUsrPosts syskeys usr1 usr2
                    case maybePosts of 
                        Just posts -> do 
                            liftIO $ print posts
                        Nothing -> 
                            liftIO $ putStrLn "Not connected or posts could not be parsed" 
