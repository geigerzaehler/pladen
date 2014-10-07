{-|
Module      : Pladen.App.Environment
Copyright   : Thomas Scholtes, 2014
License     : MIT
Maintainer  : thomas-scholtes@gmx.de
Stability   : experimental
-}
module Pladen.App.Environment (
    Environment(..)
  , environment
  , settings
  , Config(..)
) where

import Control.Applicative

import Pladen.Config (Config(..))

import Data.Set hiding (map)
import Data.Text hiding (map, lines)
import System.IO (Handle, openFile, IOMode(..))
import Database.Persist.Sql (ConnectionPool)
import qualified Data.ByteString as BS

import Database.Persist.Sqlite (createSqlitePool)
import System.FilePath
import Network.Wai.Handler.Warp (Settings, setPort, setHost, defaultSettings)
import Data.ByteString.UTF8 (toString)

data Environment = Environment {
    -- | Set of music file paths that exists on the server
      files :: Set FilePath
    , db :: ConnectionPool
    , requestLog :: Handle
    , config :: Config
    }


environment :: Config -> IO Environment
environment conf = do
    let dbpath = beetsDb conf
    pool <- createSqlitePool (pack dbpath) 5
    fs <- fromList <$> map (beetsDir conf </>)
                   <$> lines
                   <$> toString
                   <$> BS.readFile (musicDir conf </> "server.list")
    requestLog' <- openFile "album_requests.log" AppendMode
    return $ Environment fs pool requestLog' conf


-- | Return settings to start up the Warp server with
-- 'Network.Wai.Handler.Warp.runSettings'.
settings :: Environment -> Settings
settings env =
    setPort port'
  . setHost host
  $ defaultSettings
      where
          port' = port . config $ env
          host | isDev . config $ env = "*"
               | otherwise  = "127.0.0.1"
