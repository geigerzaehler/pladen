{-# LANGUAGE OverloadedStrings #-}

-- | Exports a 'Controller' that streams an album as a /tar/
-- archive.
module Pladen.App.TarAlbum
    (
      tarAlbum
    ) where

import Control.Applicative
import qualified Data.ByteString.Char8 as BS

import qualified System.FilePath.Posix as Posix
import System.FilePath (takeFileName)

import Network.HTTP.Types.Header

import Pladen.Beets.Persist
import Pladen.App.Controller
import Pladen.App.Tar


tarAlbum :: EntityAlbumTracks -> Controller
tarAlbum = response . archive


response :: (String, Archive) -> Controller
response (name, a) = request
                 >>= liftIO . tarResponse (contentDisp $ name ++ ".tar") a
                 >>= respond


contentDisp :: String -> ResponseHeaders
contentDisp name = [("Content-Disposition"
                   , BS.pack $ "attachment; " ++
                               "filename=\"" ++ name ++ "\"")]


archive :: EntityAlbumTracks -> (String, Archive)
archive (EntityAlbumTracks album tracks) =
    (root, tarPath . trackPath . entityVal <$> tracks)
    where
        root = albumName $ entityVal album
        tarPath :: FilePath -> (FilePath, FilePath)
        tarPath p = (p, root Posix.</> takeFileName p)
