{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Post.Actions (
  createPost,
) where

import Network.HTTP.Types (Status)
import qualified Network.HTTP.Simple as NS
import qualified Data.ByteString.Char8 as B
-- import Network.HTTP.Client (requestBody)
-- import qualified Data.ByteString.Lazy.Char8 as LB
-- import Data.Text (Text, unpack)
-- import Data.Aeson

import qualified Post.Types as PT 
import qualified Syskeys as SK

createPost :: SK.SysKeys -> PT.Post -> IO Status
createPost sk post = do
    let 
        method = NS.parseRequest_ $ "POST " <> SK.neureloEndpoint sk <> "/rest/POSTS/__one"
        request' = NS.setRequestBodyJSON post $ NS.setRequestHeader "X-API-KEY" [B.pack $ SK.neureloKey sk] method
    resp <- NS.httpLBS request'
    pure $ NS.getResponseStatus resp
