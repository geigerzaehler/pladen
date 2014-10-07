{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Pladen.Beets.Persist
import Database.Persist.Sqlite (createSqlitePool)

import Data.List (groupBy)
import qualified Data.Function as F
import Database.Esqueleto hiding (groupBy)
import Database.Persist.Sql ( ConnectionPool
                            , SqlPersistM
                            , runSqlPersistMPool )

dbpath = "/home/thomas/Musik/beets.blb"

run :: SqlPersistM a -> IO a
run p = do
    pool <- createSqlitePool (dbpath) 5
    runSqlPersistMPool p pool
