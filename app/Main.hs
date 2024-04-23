{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Web.Scotty as WS
import Control.Monad.IO.Class(liftIO)
import Actions(UserW, createUser)


main ::IO()
main = do
    WS.scotty 3000 $ do
        WS.get "/" $
            WS.text "Hello World"
        WS.post "/" $ do
            usr <- WS.jsonData :: WS.ActionM UserW
            resp <- liftIO $ createUser usr 
            WS.liftIO $ putStrLn resp 
