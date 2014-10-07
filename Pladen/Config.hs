{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Pladen.Config
Description : Customize the behaviour of the application
Copyright   : Thomas Scholtes, 2014
License     : MIT
Maintainer  : thomas-scholtes@gmx.de
Stability   : experimental

The 'Config' data contains settings that allow the user to customize
the application's behavior. The 'settings' and 'environment' functions
extract the data that is necessary to start the server and the
application. 'Config' implements the 'FromJSON' interface so it can be
parsed from JSON or YAML files.
-}
module Pladen.Config (
    Config(..)
) where

import Control.Monad
import Control.Applicative

import Data.Yaml

data Config = Config {
  -- | Location of beets sqlite3 database
    beetsDb :: FilePath
  -- | TODO Location of what?
  , beetsDir :: FilePath
  -- | Directory containing the audio files referenced by the database
  , musicDir :: FilePath
  -- | Port the server listens to
  , port :: Int
  -- | Flag that enables various features useful for development
  , isDev :: Bool
} deriving (Show)


instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                         v .: "beets-db" <*>
                         v .: "beets-dir" <*>
                         v .: "music-dir" <*>
                       ( v .: "port" <|> return 3000 ) <*>
                       ( v .: "dev" <|> return False )
  parseJSON _ = mzero
