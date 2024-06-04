{-# LANGUAGE OverloadedStrings #-}

module Paths where

import qualified Web.Scotty as WS
import Control.Monad.IO.Class(liftIO)
import Network.HTTP.Types (ok200,badRequest400)
import User.Actions(createUser, isUniqueUser, deactivateUser, createConn, validateCreds, getUserId)
import User.Types(User, UsrConn, IpUsrCreds)
import Post.Actions(createPost, getUsrPosts)
import Post.Types(Post)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Syskeys (SysKeys, getKeys)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

{-# NOINLINE keys #-}
keys :: IORef SysKeys
keys = unsafePerformIO $ do
    k <- getKeys
    newIORef k

checkUnameR :: WS.ActionM ()
checkUnameR = do
    syskeys <- liftIO $ readIORef keys
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

checkUserEmailR :: WS.ActionM ()
checkUserEmailR = do
    syskeys <- liftIO $ readIORef keys
    maybeQ <- WS.queryParamMaybe "ip_user_email" :: WS.ActionM (Maybe T.Text)
    case maybeQ of
        Just param -> do
            user <- liftIO $ getUserId syskeys param
            case user of 
                Just _ -> do
                    WS.status ok200
                    WS.text "Email exists"
                Nothing -> do
                    WS.status ok200
                    WS.text "Email does not exist"
        Nothing -> do
            WS.status badRequest400
            WS.text "no input"

createUserR :: WS.ActionM ()
createUserR = do 
    syskeys <- liftIO $ readIORef keys
    usr <- WS.jsonData :: WS.ActionM User
    (respStatus, respBody) <- liftIO $ createUser syskeys usr
    WS.liftIO $ print respStatus
    WS.status respStatus
    WS.setHeader "Content-Type" "application/json"
    WS.raw respBody

deactivateUserR :: WS.ActionM ()
deactivateUserR = do 
    syskeys <- liftIO $ readIORef keys
    maybeUname <- WS.pathParamMaybe "user_id" :: WS.ActionM (Maybe Int)
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

userLoginR :: WS.ActionM ()
userLoginR = do
    syskeys <- liftIO $ readIORef keys
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

userConnectR :: WS.ActionM ()
userConnectR = do 
    syskeys <- liftIO $ readIORef keys
    usrsConn <- WS.jsonData :: WS.ActionM UsrConn
    (respStatus, respBody) <- liftIO $ createConn syskeys usrsConn 
    WS.liftIO $ print respStatus
    WS.status respStatus
    WS.setHeader "Content-Type" "application/json"
    WS.raw respBody

postCreateR :: WS.ActionM ()
postCreateR = do
    syskeys <- liftIO $ readIORef keys
    post <- WS.jsonData :: WS.ActionM Post
    (respStatus, respBody) <- liftIO $ createPost syskeys post
    WS.liftIO $ print respStatus 
    WS.status respStatus
    WS.setHeader "Content-Type" "application/json"
    WS.raw respBody

getAllPostsR :: WS.ActionM ()
getAllPostsR = do
    syskeys <- liftIO $ readIORef keys
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
