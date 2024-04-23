{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Web.Scotty as WS
import qualified Network.HTTP.Simple as NS
import Control.Monad.IO.Class(liftIO)
import Actions(UserW, createUser)
import qualified Data.Text as T 


main ::IO()
main = do
    WS.scotty 3000 $ do
        WS.get "/" $
            WS.text "EATSY!!!!!!"
        WS.post "/user/create" $ do
            usr <- WS.jsonData :: WS.ActionM UserW
            resp <- liftIO $ createUser usr
            WS.liftIO $ print resp  
            WS.status resp
