{-# LANGUAGE OverloadedStrings #-}

module Pladen.App.Feed
  (
    feed
  ) where


import Control.Applicative
import Data.Time.Format (formatTime)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Ord (comparing)
import Data.List (sortBy, sort)
import Data.Text (Text)
import Data.String (fromString)
import qualified Data.Map.Lazy as Map

import System.Locale (defaultTimeLocale)

import Text.XML

-- import Network.URI (URI(..), URIAuth(..))
import Text.URI (URI(..))


import Pladen.Beets.Persist
import Pladen.App.Controller


-- | Respond with the atom feed
feed :: Controller
feed = getDoc
   >>= return . render
   >>= respondAtom
    where
        respondAtom = respond . ok "application/atom+xml"
        render = renderLBS def

-- | Create the feed document from the database
getDoc :: Controller' Document
getDoc = buildDoc
       . buildFeed
       . map entry
       . recent <$> runPersist getAlbums'


data Feed = Feed
    { title :: String
    , link :: URI
    , updated :: UTCTime
    , author :: Author
    , feedId :: String
    , entries :: [Entry]
    }

data Entry = Entry
    { entryTitle :: String
    , entryURL :: URI
    , entryId :: String
    , entryUpdated :: UTCTime
    , entrySummary :: String
    }

type Author = String

domain :: String
domain = "maboite.org"

baseUrl :: URI
baseUrl = URI
    { uriScheme = Just "https"
    , uriUserInfo = Nothing
    , uriRegName = Just domain
    , uriPort = Nothing
    , uriPath = "/"
    , uriQuery = Nothing
    , uriFragment = Nothing
    }



----------------------------------------------------------------
-- Building 'Document' and 'Element's from 'Feed' and 'Entry'.
----------------------------------------------------------------

buildDoc :: Feed -> Document
buildDoc f = Document prologue (feedEl f) [] where
    prologue = Prologue [] Nothing []

feedEl :: Feed -> Element
feedEl f = Element (atomName "feed") Map.empty children
    where
        children = map NodeElement (
            [ linkEl $ link f
            , simpleEl "title" $ title f
            , simpleEl "updated" $ iso $ updated f
            , containerEl "author" [simpleEl "name" (author f)]
            , simpleEl "id" $ feedId f
            ] ++ map entryEl (entries f))
            

entryEl :: Entry -> Element
entryEl = Element (atomName "entry") Map.empty . children
    where
        children e = map NodeElement (
          [ simpleEl "title" . entryTitle
          , simpleEl "id" . entryId
          , simpleEl "summary" . entrySummary
          , simpleEl "updated" . iso . entryUpdated
          , linkEl . entryURL
          ] <*> pure e)

simpleEl :: String -> String -> Element
simpleEl name content = Element
                          (atomName name)
                          Map.empty
                          [NodeContent $ fromString content]

containerEl :: String -> [Element] -> Element
containerEl name els = Element (atomName name) Map.empty (map NodeElement els)

linkEl :: Show a => a -> Element
linkEl url = Element (atomName "link") (Map.singleton "href" url') []
    where url' = fromString $ show url

atomName :: String -> Name
atomName local = Name (fromString local) (Just atomNS) Nothing

atomNS :: Text
atomNS = "http://www.w3.org/2005/Atom"

iso :: UTCTime -> String
iso = formatTime defaultTimeLocale "%FT%TZ"



----------------------------------------------------------------
-- Creating 'Feed' and 'Entry' from 'Album' list
----------------------------------------------------------------

buildFeed :: [Entry] -> Feed
buildFeed es = Feed
    { title = "Thomas’ Musik"
    , link  = baseUrl
    , updated = head . reverse . sort $ map entryUpdated es
    , author = "Thomas Scholtes"
    , feedId  = "https://mu.scholtes-weingut.de"
    , entries = es
    }

entry :: Album -> Entry
entry a = Entry
    { entryTitle = albumName a
      -- FIXME should not be fragment
    , entryURL = baseUrl { uriFragment = Just $ "/new/" ++ albumName a }
    , entryId = show $ baseUrl { uriFragment = Just $ "/new/" ++ albumName a }
    , entryUpdated = posixSecondsToUTCTime . realToFrac . albumAdded $ a
    , entrySummary = "\"" ++ albumName a 
                  ++ "\" von " ++ albumArtist a 
    }

-- | Get the 20 most recently added albums
recent :: [Album] -> [Album]
recent = take 20 . reverse . sortBy (comparing albumAdded)
