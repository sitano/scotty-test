{-# LANGUAGE OverloadedStrings #-}
module Query (connect, setup, addUrl, getUrl, listUrl) where

import Control.Monad(unless)

import Database.HDBC.MySQL
import Database.HDBC

data Url =
    Url {urlId :: Integer,  -- ^ Numeric ID for this URL
         urlPath :: String  -- ^ URL path
        }
    deriving (Eq, Show, Read)

connect :: MySQLConnectInfo -> IO Connection
connect = connectMySQL

setup :: IConnection conn => conn -> IO ()
setup db =
  do sql <- readFile "data.sql"
     tables <- getTables db
     unless ("url" `elem` tables) $
       do run db sql []
          return ()
     commit db

addUrl :: IConnection conn => conn -> Url -> IO Url
addUrl db url = error "not implemented"

getUrl :: IConnection conn => conn -> Int -> IO Url
getUrl db url = error "not implemented"

listUrl :: IConnection conn => conn -> IO [Url]
listUrl db = error "not implemented"
