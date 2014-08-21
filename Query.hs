{-# LANGUAGE OverloadedStrings #-}
module Query (connect, setup, addUrl, getUrl, listUrl) where

import Control.Monad(unless)

import Database.HDBC.MySQL
import Database.HDBC

data Url =
    Url {urlId :: Int,      -- ^ Numeric ID for this URL
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

addUrl :: IConnection conn => conn -> String -> IO Url
addUrl db url =
  handleSql err $ do
    run db "INSERT INTO `url` (`url`) VALUES (?)" [ toSql url ]
    r <- quickQuery' db "SELECT LAST_INSERT_ID() as `id`" []
    case r of
      [[x]] -> return Url {urlId = fromSql x, urlPath = url}
      y -> fail $ "addUrl: unexpected result: " ++ show y
    where
      err e = fail $ "Error adding url\n" ++ show e

getUrl :: IConnection conn => conn -> Int -> IO Url
getUrl db uid =
  do
    r <- quickQuery' db "SELECT `url` FROM `url` WHERE `id` = ?" [ toSql uid ]
    case r of
      [[x]] -> return Url {urlId = uid, urlPath = fromSql x}
      _ -> fail "no url by id"

listUrl :: IConnection conn => conn -> IO [Url]
listUrl db =
  do
    r <- quickQuery' db "SELECT `id`, `url` FROM `url`" []
    return (map conv r)
  where
    conv [x, y] = Url (fromSql x) (fromSql y)
