{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Web.Scotty as WS
import Control.Monad.IO.Class(liftIO)
import Actions(User, createUser, isUniqueUser)
import qualified Data.Text as T


main ::IO()
main = do
    WS.scotty 3000 $ do
        WS.get "/" $
            WS.text "EATSY!!!!!!"
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
