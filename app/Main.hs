{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Web.Scotty as WS
import Control.Monad.IO.Class(liftIO)
import User.Actions(createUser, isUniqueUser, deactivateUser)
import User.Types(User)
import qualified Data.Text as T


main ::IO()
main = do
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
                    resp <- liftIO $ isUniqueUser param                               
                    WS.liftIO $ print resp  
                    WS.status resp
                Nothing -> liftIO $ putStrLn "no input"
        WS.post "/user/create" $ do
            usr <- WS.jsonData :: WS.ActionM User
            resp <- liftIO $ createUser usr
            WS.liftIO $ print resp  
            WS.status resp
        WS.patch "/user/deactivate/username/:username" $ do
            maybeUname <- WS.pathParamMaybe "username" :: WS.ActionM (Maybe T.Text)
            case maybeUname of
                Just uname -> do 
                    resp <- liftIO $ deactivateUser uname
                    WS.liftIO $ print resp  
                    WS.status resp
                Nothing -> liftIO $ putStrLn "no input"
