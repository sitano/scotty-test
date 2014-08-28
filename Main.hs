{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Network.Wai.Middleware.RequestLogger
import Database.HDBC
import Database.HDBC.MySQL
import Web.Scotty

import Args
import Query

main :: IO ()
main = do
  args <- parseArgs

  db <- connect defaultConnectInfo {
    mysqlHost = host args,
    mysqlUser = user args,
    mysqlPassword = password args,
    mysqlDatabase = database args
    }

  scotty (port args) $ do
    middleware logStdoutDev

    get "/" $
      html $ renderHtml
        $ H.html $
          H.body $
            H.p "Simple REST url shortener service example implemented in haskell"

    post "/add" $ param "url" >>= \p -> liftIO (addUrl db p >>= \u -> commit db >> return u) >>= json

    get "/list" $ liftIO (listUrl db) >>= json

    get "/:hash" $ param "hash" >>= \h -> liftIO (getUrl db h) >>= redirect . T.pack . urlPath

    liftIO $ putStrLn ("Simple REST url shortener service example implemented in haskell. " ++
                       "Run at " ++ show (port args))
