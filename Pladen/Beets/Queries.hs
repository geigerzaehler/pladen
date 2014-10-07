{-# LANGUAGE GADTs #-}

module Pladen.Beets.Queries
    (
      get
    , getAll
    , getAlbums, getAlbums'
    , getAlbumTrack
    , getAlbumTracks

    , getTrack
    , getSingletonTracks

    , getArtists
    ) where

import Control.Applicative
import qualified Data.List as L
import qualified Data.Function as F

import Database.Persist.Class
import Database.Persist.Types
import Database.Esqueleto

import Pladen.Beets.Model


getAll :: ( PersistEntity val
          , PersistQuery m
          , PersistEntityBackend val ~ PersistMonadBackend m )
       => m [Entity val]
getAll = selectList [] []

getAlbums :: SqlPersistM [Entity Album]
getAlbums = selectList [] [Desc AlbumAdded]

getAlbums' :: SqlPersistM [Album]
getAlbums' = map entityVal <$> getAlbums

----------------------------------------------------------------------------


getAlbumTrack :: Key Album -> SqlPersistM (Maybe EntityAlbumTracks)
getAlbumTrack key = do
    tracks <- select $ 
        from $ \(t `InnerJoin` a) -> do
        on     $ a ^. AlbumId ==. t ^. TrackAlbumId
        where_ $ a ^. AlbumId ==. val key
        orderBy [asc $ t ^. TrackNo]
        return (a,t)

    return $ case tracks of
        [] -> Nothing
        _  -> Just $ entityAlbumTracks tracks


getAlbumTracks :: SqlPersistM [EntityAlbumTracks]
getAlbumTracks = collectAlbumTracks <$> joinAlbumTracks


joinAlbumTracks :: SqlPersistM [(Entity Album, Entity Track)]
joinAlbumTracks = select $
     from $ \(t `InnerJoin` a) -> do
     on (a ^. AlbumId ==. t ^. TrackAlbumId)
     orderBy [asc (t ^. TrackAlbumId)]
     return (a,t)


collectAlbumTracks :: [(Entity Album, Entity Track)] -> [EntityAlbumTracks]
collectAlbumTracks = (map entityAlbumTracks) . group
    where
        group = L.groupBy ((==) `F.on` (entityKey .fst))


entityAlbumTracks :: [(Entity Album, Entity Track)] -> EntityAlbumTracks
entityAlbumTracks es = EntityAlbumTracks (fst $ head es) (map snd es)

----------------------------------------------------------------------------

getTrack :: Key Track -> SqlPersistM (Maybe (Entity Track))
getTrack k = (fmap.fmap) (Entity k) $ get k

getSingletonTracks :: SqlPersistM [Entity Track]
getSingletonTracks = selectList [filterSingletons] []
    where filterSingletons = Filter TrackAlbumId (Left $ Key PersistNull) Eq

----------------------------------------------------------------------------

getArtists :: SqlPersistM [EntityArtist]
getArtists = collectArtists <$> selectList [] [Asc AlbumArtist, Desc AlbumYear]

collectArtists :: [Entity Album] -> [EntityArtist]
collectArtists albums = map artistFromAlbums grouped
    where
        artistFromAlbums as = EntityArtist (albumArtist $ entityVal $ head as)
                                           (map entityKey as)
        grouped :: [[Entity Album]]
        grouped = L.groupBy ((==) `F.on` (albumArtist . entityVal)) albums
