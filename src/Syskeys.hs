module Syskeys (
  SysKeys,
  neureloKey,
  neureloEndpoint,
  getKeys
) where 

import System.Environment

data SysKeys = SysKeys
    { neureloKey :: String,
      neureloEndpoint :: String
    } deriving (Show)

getKeys :: IO SysKeys
getKeys = do 
    nKey <- getEnv "NEURELO_KEY"
    nApi <- getEnv "NEURELO_ENDPOINT"
    let syskeys = SysKeys nKey nApi
    pure syskeys 
