{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Post.Types (
    Post(..),
    OPPost(..)
) where

import Data.Aeson
import GHC.Generics
import Data.Text (Text, strip)

data Post = Post {
    title :: Text,
    steps :: Text,
    url :: Text,
    userId :: Int
} deriving (Generic, Show)

instance FromJSON Post where
    parseJSON = withObject "Post" $ \obj -> do
        title_ <- strip <$> obj .: "title"
        steps_ <- strip <$> obj .: "steps"
        url_ <- strip <$> obj .: "url"
        userId_ <- obj .: "appuser_user_id"
        return $ Post title_ steps_ url_ userId_
-- instance FromJSON Post

instance ToJSON Post where 
    toJSON (Post title_ steps_ url_ userId_) = 
        object [
            "title" .= title_, 
            "steps" .= steps_,
            "url" .= url_, 
            "APPUSER" .= object [ 
                "connect" .= object ["user_id" .= userId_]
              ]
            ]

data OPPost = OPPost{
    post_id ::Int,
    appuser_user_id :: Int,
    created_at :: Text,
    opTitle :: Text,
    opSteps :: Text,
    opUrl ::Text
} deriving (Generic, Show)
instance FromJSON OPPost where
    parseJSON = withObject "OPPost" $ \obj -> do
        post_id_ <- obj .: "post_id"
        appuser_user_id_ <- obj .: "appuser_user_id"
        created_at_ <- obj .: "created_at"
        opTitle_ <- obj .: "title"
        opSteps_ <- obj .: "steps"
        opUrl_ <- obj .: "url"
        return $ OPPost post_id_ appuser_user_id_ created_at_ opTitle_ opSteps_ opUrl_
instance ToJSON OPPost where 
    toJSON (OPPost post_id_ appuser_user_id_ created_at_ opTitle_ opSteps_ opUrl_) =
        object [
            "post_id" .= post_id_,
            "appuser_user_id" .= appuser_user_id_,
            "created_at" .= created_at_,
            "title" .= opTitle_,
            "steps" .= opSteps_,
            "opUrl" .= opUrl_
        ]
