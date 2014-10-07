{-|
Module      : Pladen.App.Controller
Description : Basic building blocks of the web server
Copyright   : Thomas Scholtes, 2014
License     : MIT
Maintainer  : thomas-scholtes@gmx.de
Stability   : experimental
-}
module Pladen.App.Controller
    (
      Controller, Controller'
    , runPersist
    , getUploadedFiles
    , getConfig
    , putRequestLog

    , respond
    , request
    , queryParam, queryParam'
    , readQueryParam, readQueryParam'
    , routePattern, route
    , controllerApp, fromApp

    , tryM

    , module Web.Simple.Responses

    , module Control.Applicative
    , liftIO
    ) where

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get)
import Data.Set
import Data.Text

import System.IO (hPutStr, hFlush)
import Database.Persist.Sql ( SqlPersistM
                            , runSqlPersistMPool )

import Web.Simple.Controller hiding (Controller)
import Web.Simple.Responses
import Pladen.App.Environment


-- | The monad we build the webserver with.
type Controller' a = ControllerT Environment IO a

-- | Shortcut for the 'Controller'' monad with no return value.
type Controller  = Controller' ()


-- | Run a persistence operation in the 'Controller''.
runPersist :: SqlPersistM a -> Controller' a
runPersist p = do
    pool <- db <$> get
    liftIO $ runSqlPersistMPool p pool


-- | Get the set of music file paths that exist on the server.
getUploadedFiles :: Controller' (Set FilePath)
getUploadedFiles = files <$> get


getConfig :: Controller' Config
getConfig = config <$> get


-- | Put the string into the request log file.
putRequestLog :: String -> Controller' ()
putRequestLog s = do
    h <- requestLog <$> get
    liftIO $ do hPutStr h s
                hFlush h

route :: Text -> Controller' a -> Controller
route = routePattern


tryM :: Maybe a -> Controller' a
tryM = maybe mzero return
