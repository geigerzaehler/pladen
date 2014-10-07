{-# LANGUAGE
    QuasiQuotes
  , TypeFamilies
  , GeneralizedNewtypeDeriving
  , TemplateHaskell
  , OverloadedStrings
  , GADTs
  , TypeSynonymInstances 
  , FlexibleInstances #-}

module Pladen.Beets.Model where

import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Database.Persist.TH
import Database.Persist (Key, Entity, entityIdToJSON)


mkPersist sqlSettings [persistLowerCase|
Album sql=albums
    name String sql=album
    artist String sql=albumartist
    year Int sql=original_year
    added Double sql=added
    deriving Show
Track sql=items
    title String
    path String
    artist String
    albumId AlbumId
    length Double
    format String
    no Int sql=track
    added Double
    deriving Show
|]

albumAdded' :: Album -> UTCTime
albumAdded' = posixSecondsToUTCTime . realToFrac . albumAdded

data AlbumTracks = AlbumTracks Album [Track]
    deriving Show
data EntityAlbumTracks = EntityAlbumTracks (Entity Album) [Entity Track]
    deriving Show


data EntityArtist = EntityArtist {
    artistName :: String,
    artistAlbums :: [Key Album]
} deriving (Show)



instance ToJSON Album where
    toJSON a = object
        [ "artist" .= albumArtist a
        , "name"   .= albumName a
        , "year"   .= albumYear a
        , "added"  .= albumAdded a
        ]

instance ToJSON Track where
    toJSON a = object
        [ "title"   .= trackTitle a
        , "artist"  .= trackArtist a
        , "albumid" .= trackAlbumId a
        , "no"      .= trackNo a
        , "length"  .= trackLength a
        , "added"   .= trackAdded a
        ]

instance ToJSON (Entity Track) where
    toJSON = entityIdToJSON 

instance ToJSON EntityArtist where
    toJSON a = object
        [ "name"   .= artistName a
        , "albums" .= artistAlbums a
        ]

instance ToJSON EntityAlbumTracks where
    toJSON (EntityAlbumTracks a ts) =
        case entityIdToJSON a of
            Object a' -> Object $ HM.insert "tracks" tracks a'
            x -> x
        where tracks = toJSON $ map entityIdToJSON ts
