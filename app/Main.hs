{-# LANGUAGE OverloadedStrings #-}

import qualified Web.Scotty as WS
import Paths

main ::IO()
main = do
    WS.scotty 3000 $ do
        WS.get "/" $
            WS.text "EATSY!!!!!!"

        WS.get "/check/uname" checkUnameR
        WS.get "/check/useremail" checkUserEmailR     
        WS.post "/user/create" createUserR
        WS.patch "/user/deactivate/user_id/:user_id" deactivateUserR        
        WS.post "/login" userLoginR
        WS.post "/user/connect" userConnectR

        WS.post "/post/create" postCreateR
        WS.get "/post/getall" getAllPostsR
