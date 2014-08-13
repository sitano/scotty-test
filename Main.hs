{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Console.GetOpt
import System.Environment
import System.Exit
import Control.Monad
import Data.List
import Data.Foldable

import Web.Scotty

-- import Control.Concurrent.MVar
import Control.Monad.IO.Class

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
    liftIO $ putStrLn ("Hello from scotty at " ++ (show $ port args))
