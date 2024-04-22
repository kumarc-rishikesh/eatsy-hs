{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import qualified Web.Scotty as WS
import Data.Text (Text)
import Data.Aeson 
import Network.HTTP.Simple as NS
import System.Environment
import qualified Data.ByteString.Char8 as B
-- import qualified Data.Text.IO as T
-- import Control.Lens
-- import Control.Monad (forM)
import Control.Monad.IO.Class(liftIO)
-- import Control.Concurrent.Async
-- import Data.Aeson.Types (Parser)
-- import qualified Data.ByteString.Lazy.Char8 as L8


data User = User {
    user_id :: Int,
    username :: Text,
    user_email :: Text,
    user_pw :: Text,
    dob :: Text,
    country :: Text
} deriving (Generic, Show)

instance FromJSON User
instance ToJSON User
    
main ::IO()
main = do
    WS.scotty 3000 $ do
        WS.get "/" $
            WS.text "Hello World"
        WS.post "/" $ do
            usr <- WS.jsonData :: WS.ActionM User 
            resp <- liftIO $ createUser usr 
            WS.liftIO $ putStrLn resp 

createUser :: User -> IO String 
createUser usr = do
    neureloKey <- getEnv "NEURELO_KEY"
    let 
        method = "POST https://us-east-2.aws.neurelo.com/rest/APPUSER/__one?"        
        request = setRequestBodyJSON usr $ setRequestHeader "X-API-KEY" [B.pack neureloKey] $ method
    resp <- httpLBS request 
    pure $ show $ getResponseStatus resp
