{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Console.GetOpt
import System.Environment
import System.Exit

import Control.Monad
import Control.Monad.IO.Class

import Data.List
import Data.Foldable
-- import Data.Monoid (mconcat)
-- import qualified Data.Text.Lazy as T
import Data.Aeson.Types

import qualified Text.Blaze.Html5 as H
-- import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger

import Web.Scotty

data Flags = Flags {
  port :: Int
  } deriving Show

defaults :: Flags
defaults = Flags {
  port = 8000
  }

options :: String -> [OptDescr (Flags -> IO Flags)]
options flags =
  [ Option "h" ["help"]
    (NoArg (\_ -> do
               putStr flags
               exitSuccess))
    "print usage information"
  , Option "p" ["port"]
    (ReqArg (\str opts -> do
                let g = read str
                when (g < 1 || g > 65535) $ do
                  putStrLn "Error: PORT not in the range [1, 65535]"
                  putStr flags
                  exitFailure
                return $ opts { port = g }) "PORT")
    "REST API port"
  ]


parseArgs :: IO Flags
parseArgs = do
  argv <- getArgs
  progName <- getProgName
  let usage = "Usage: " ++ progName ++ " [OPTION...]"
  let helpMessage = usageInfo usage (options "")
  case getOpt RequireOrder (options helpMessage) argv of
    (opts, [], []) -> foldlM (flip id) defaults opts
    (_, _, errs) -> ioError (userError (Data.List.concat errs ++ helpMessage))

main :: IO ()
main = do
  args <- parseArgs
  scotty (port args) $ do
    middleware logStdoutDev

    get "/" $
      html $ renderHtml
        $ H.html $
          H.body $
            H.p "Simple REST url shortener service example implemented in haskell"

    post "/shorten" $ do
      -- url <- param "url"
      status status403
      html "Not implemented yet"

    get "/list" $
      json emptyArray

    get "/:hash" $ do
      status status403
      html "Not implemented yet"
      -- hash <- param "hash"
      --raise $ mconcat ["URL hash #", T.pack $ show $ hash, " not found in database!"]

    liftIO $ putStrLn ("Simple REST url shortener service example implemented in haskell. " ++
                       "Run at " ++ (show $ port args))
