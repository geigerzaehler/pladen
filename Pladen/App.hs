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
import Data.Monoid (mappend, mconcat, (<>))
import Data.Set (Set, toList)
import Data.ByteString.Builder

import System.FilePath (splitExtension, addExtension)

import qualified Network.HTTP.Types.Status as H
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
    . static "./static"
    . index
    $ main
    where
        main :: Application
        main = controllerApp env $ do
            routePattern "assets" $ fromApp $ assets env
            -- TODO us this used?
            routePattern "files" $ do
                fs <- uploadedFiles <$> getUploadedFiles
                respond $ okHtml $ toLazyByteString fs
            api

        uploadedFiles :: Set FilePath -> Builder
        uploadedFiles = mconcat . map line . toList
            where line p = stringUtf8 p <> charUtf8 '\n'

        index :: Middleware
        index = Static.staticPolicy $ Static.only [("", "./index.html")]


-- | Application that serves files from the "assets" directory.
--
-- If "file.js" is requested it will serve "file.min.js" if that file
-- exists.
--
-- If the environment is in development mode it will also serve files
-- from the "./client" directory and prefer "file.dev.js" when
-- requesting "file.js".
assets :: Environment -> Application
assets env = devMinRewrite
           . staticClient
           . static "./assets"
           $ notFound
    where
        isDev' = isDev $ config env
        devMinRewrite | isDev'     = staticRewrite (addExt "dev") "./assets"
                      | otherwise  = staticRewrite (addExt "min") "./assets"
        staticClient  | isDev'     = static "./client"
                      | otherwise  = id


-- | Sends an empty response with a 404 status code
notFound :: Application
notFound = const $ return $ responseLBS H.status404 [] ""

static :: String -> Middleware
static = Static.staticPolicy . Static.addBase

staticRewrite :: (String -> String) -> FilePath -> Middleware
staticRewrite rewrite base = Static.staticPolicy $
                             Static.addBase base
                   `mappend` Static.policy (Just . rewrite)

addExt :: FilePath -> String -> FilePath
addExt ext path = let (base, oldExt) = splitExtension path
                      extBase = addExtension base ext
                  in extBase ++ oldExt
