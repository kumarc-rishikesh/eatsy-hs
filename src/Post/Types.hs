{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Post.Types (
    Post(..)
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
