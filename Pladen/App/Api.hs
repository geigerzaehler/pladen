{-# LANGUAGE OverloadedStrings #-}

module Pladen.App.Api (
    api
) where

import Control.Monad
import Text.Read (readMaybe)

import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import Data.Text (Text)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

import Data.Aeson (encode, (.=), object, toJSON, ToJSON)
import Data.Aeson.Types (Pair, Value(..))


import Web.Frank
import Pladen.App.Controller

import Pladen.Beets.Persist hiding (get)
import Pladen.App.TarAlbum (tarAlbum)
import Pladen.App.Feed (feed)
import Pladen.App.Stream (streamTrack)


api :: Controller
api = do
    get "feed.xml" feed
                          
    route "album" $ do
        get "" $ runPersist getAlbumTracks >>= sendAlbums
        withAlbum $ \album -> do
            let EntityAlbumTracks albumE _ = album
            get   ""        $ sendObject ["albums" .= [album]]
            route "tar"     $ tarAlbum album
            put   "request" $ do
                let keyString = unpack $ encode $ entityKey albumE
                time <- liftIO getTimestamp
                putRequestLog $ time ++ ";" ++ keyString ++ ";"
                             ++ albumArtist (entityVal albumE) ++ ";"
                             ++ albumName (entityVal albumE) ++ "\n"
                respond $ okHtml ""

    route "track" $ do
        get "singleton" $ do
            tracks <- runPersist getSingletonTracks
            sendTracks tracks
        withTrack $ \track -> do
            get "" $ sendObject ["tracks" .= [track]]
            get "file" $ streamTrack $ entityVal track



withAlbum :: (EntityAlbumTracks -> Controller) -> Controller
withAlbum c = routePattern ":id" $ do
    albumId <- idParam
    album <- runPersist $ getAlbumTrack albumId
    maybe mzero c album


withTrack :: (Entity Track -> Controller) -> Controller
withTrack c = routePattern ":id" $ do
    trackId <- idParam
    track <- runPersist $ getTrack trackId
    maybe mzero c track


-- | Return the corresponding database key if the @id@ query parameter
-- is given. Otherwise abort the computation.
idParam :: Controller' (Key a)
idParam = do
    mid <- queryParam "id"
    let mkey = Key <$> PersistInt64 <$> (readMaybe =<< mid)
    maybe mzero return mkey


-- | Send response with the serialized albums
sendAlbums :: [EntityAlbumTracks] -> Controller
sendAlbums albumTracks = do
    uploaded <- getUploadedFiles
    sendObject [ "albums" .= map (jsonDownloadable uploaded) albumTracks ]


-- | Send response with the serialized tracks
sendTracks :: [Entity Track] -> Controller
sendTracks tracks = do
    uploaded <- getUploadedFiles
    sendObject [ "tracks" .= map (jsonDownloadableTrack uploaded) tracks ]


-- TODO where to put this
jsonDownloadable :: S.Set FilePath -> EntityAlbumTracks -> Value
jsonDownloadable available (EntityAlbumTracks album tracks)
    = extend (entityIdToJSON album) "downloadable" (isDownloadable available tracks)


jsonDownloadableTrack :: S.Set FilePath -> Entity Track -> Value
jsonDownloadableTrack available track
    = extend (entityIdToJSON track) "downloadable" (isDownloadable available [track])


extend :: (ToJSON a, ToJSON v) => a -> Text -> v -> Value
extend a k v | Object o <- toJSON a
             = Object $ H.insert k (toJSON v) o
             | otherwise = toJSON a

-- | Return 'True' if all of the tracks' paths are contained in the
-- path set.
isDownloadable :: S.Set FilePath -> [Entity Track] -> Bool
isDownloadable set = all (flip S.member set . trackPath . entityVal)


sendObject :: [Pair] -> Controller
sendObject = respond . okJson . encode . object


getTimestamp :: IO String
getTimestamp = format <$> getZonedTime
    where format = formatTime defaultTimeLocale "%FT%T%z"
