{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Main entry point for the applictaion
Copyright   : Thomas Scholtes, 2014
License     : MIT
Maintainer  : thomas-scholtes@gmx.de
Stability   : experimental

This module contains the main entry point for the executable. It reads the
configuration and fires up the server.
-}

module Main where

import Control.Applicative

import Network.Wai.Handler.Warp
import Data.Streaming.Network.Internal (HostPreference(Host))

import Options.Applicative as O
import Data.Yaml

import Pladen.App (app)
import Pladen.App.Environment


-- | Gets the 'Config' from the command line arguments, sets up the application
-- 'Env' and starts the server.
main :: IO ()
main = do
    env <- environment =<< cliConfig
    let settings' = settings env
    putStrLn (startMessage settings')
    runSettings settings' $ app env


startMessage :: Settings -> String
startMessage settings' = "Starting pladen at " ++ host ++ ":" ++ port'
    where
        host = case getHost settings' of
            Host h -> h
            other  -> show other
        port' = show $ getPort settings'

-- | Parse the command line to obtain the application configuration
-- TODO error handling if file does not exist
cliConfig :: IO Config
cliConfig = do
    args <- parseArguments
    conf <- decodeFile $ configLocation args
    case conf of
      Nothing -> error "Could not parse config"
      Just conf' -> return conf'


data CLIArguments = CLIArguments {
    configLocation :: FilePath
  }

argumentParser :: O.Parser CLIArguments
argumentParser = CLIArguments <$> strOption
        ( long "config"
       <> short 'c'
       <> help "Path to a YAML configuration file"
       <> metavar "CONFIG"
       <> value "pladen.conf")

parseArguments :: IO CLIArguments
parseArguments = execParser $ info
    (helper <*> argumentParser)
    briefDesc
