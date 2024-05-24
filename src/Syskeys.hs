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
    nKey <- readFile "./neurelo_key"
    let nApi = "https://us-east-2.aws.neurelo.com"
    let syskeys = SysKeys nKey nApi
    pure syskeys 
