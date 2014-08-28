{-# LANGUAGE OverloadedStrings #-}
module Args(Flags,
            port, host, user, password, database,
            parseArgs) where

import System.Console.GetOpt
import System.Environment
import System.Exit

import Control.Monad

import Data.List
import Data.Foldable

data Flags = Flags {
  port :: Int,
  host :: String,
  user :: String,
  password :: String,
  database :: String
  } deriving Show

defaults :: Flags
defaults = Flags {
  port = 8000,
  host = "127.0.0.1",
  user = "root",
  password = "",
  database = "short"
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
  , Option "" ["host"]
    (ReqArg (\str opts ->
                return $ opts { host = str }) "HOST")
    "MySQL host (127.0.0.1 by default)"
  , Option "u" ["user"]
    (ReqArg (\str opts ->
                return $ opts { user = str }) "USER")
    "MySQL username (root by default)"
  , Option "" ["password"]
    (ReqArg (\str opts ->
                return $ opts { password = str }) "PASSWORD")
    "MySQL password (empty by default)"
  , Option "d" ["database"]
    (ReqArg (\str opts ->
                return $ opts { database = str }) "DATABASE")
    "MySQL service database (short by default)"
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
