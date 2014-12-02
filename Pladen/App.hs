{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Pladen.App
Description : Build the server application from the 'Environment'
Copyright   : Thomas Scholtes, 2014
License     : MIT
Maintainer  : thomas-scholtes@gmx.de
Stability   : experimental
-}

module Pladen.App (
    app
) where

import Control.Applicative
import Data.List (stripPrefix)
import qualified Data.Text as T

import System.FilePath ( splitExtension, addExtension
                       , splitPath, joinPath )

import           Network.Wai
import           Network.Wai.Middleware.Gzip (gzip, def)
import qualified Network.Wai.Middleware.Static as Static

import Pladen.App.Api (api)
import Pladen.App.Controller hiding (notFound)
import Pladen.App.Environment


-- | Creates the web application.
--
-- The application servers static files and assets and provides the
-- JSON 'api'.
app :: Environment -> Application
app env =
      gzip def
    . serveFiles env
    $ controllerApp env api


serveFiles :: Environment -> Middleware
serveFiles env = index
               . static "./static"
               . mount "assets" assets
               . mount "test" (dev test)
    where
        isDev' = isDev $ config env

        dev :: Middleware -> Middleware
        dev m = if isDev' then m else id

        index :: Middleware
        index = staticOnly "" "./index.html"

        test :: Middleware
        test = static "./test/interact"
             . staticOnly "" "./test/interact/index.html"

        -- | Application that serves files from the "assets" directory.
        --
        -- If "file.js" is requested it will serve "file.min.js" if that file
        -- exists.
        --
        -- If the environment is in development mode it will also serve files
        -- from the "./client" directory and prefer "file.dev.js" when
        -- requesting "file.js".
        assets :: Middleware
        assets = tryRewrite (addExt ext) (static "./assets")
               . dev (static "./client")

        ext = if isDev' then "dev" else "min"



mount :: FilePath -> Middleware -> Middleware
mount path m fallback req =
    maybe (fallback req)
          (\p -> m fallback $ req {pathInfo = p})
          (stripPrefix path' $ pathInfo req)
    where path' = T.pack <$> splitPath path


static :: String -> Middleware
static = Static.staticPolicy . Static.addBase


staticOnly :: String -> String -> Middleware
staticOnly p t = Static.staticPolicy $ Static.only [(p, t)]


tryRewrite :: (FilePath -> FilePath) -> Middleware -> Middleware
tryRewrite t m fallback req = m original $ req {pathInfo = rewritten}
    where
        original :: Application
        original _ = m fallback req
        rewritten = fmap T.pack . splitPath
                  . t . joinPath . fmap T.unpack . pathInfo $ req


addExt :: String -> FilePath -> FilePath
addExt ext path = let (base, oldExt) = splitExtension path
                      extBase = addExtension base ext
                  in extBase ++ oldExt
