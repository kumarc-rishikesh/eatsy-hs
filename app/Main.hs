{-# LANGUAGE OverloadedStrings #-}

import qualified Web.Scotty as WS
import Control.Monad.IO.Class(liftIO)
import User.Actions(createUser, isUniqueUser, deactivateUser, createConn)
import User.Types(User, UsrConn)
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
 
