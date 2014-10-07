{-# LANGUAGE OverloadedStrings #-}

module Pladen.App.Tar
    (
      tarResponse
    , Archive
    ) where

import Control.Applicative
import Data.Maybe
import Data.Monoid

import Data.Char (ord, isDigit)
import Data.Text.Encoding (decodeUtf8)

import           Blaze.ByteString.Builder (Builder, fromByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Conduit
import qualified Data.Conduit.List as CL

import Text.Parsec.Text (Parser)
import Text.Parsec.Char
import Text.Parsec (parse)

import Network.Wai

import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method

import Pladen.Tar


tarResponse :: ResponseHeaders
            -- ^ Additional headers for the reponse
            -> Archive
            -> Request
            -> IO Response
tarResponse h a req =
    responseSourceBracket (openEntries a)
                          teardown
                          (respond defFallback h req)


defFallback :: BracketResponse
defFallback = (status404, [], mempty)


-- | Close all handles
teardown :: Maybe [FileEntryHandle] -> IO ()
teardown = maybe (return ()) (mapM_ closeEntry)


type BracketResponse = (Status, ResponseHeaders, Source IO (Flush Builder))

respond :: BracketResponse
        -- ^ Fallback response if file entries
        -- could not be opened
        -> ResponseHeaders
        -- ^ Additional headers for tar response
        -> Request
        -> Maybe [FileEntryHandle]
        -> IO BracketResponse
respond fb h req = maybe (return fb) (fmap modify . respond' req)
    where modify (st, h', src) = (st, h' ++ h, src)
      

respond' :: Request
         -> [FileEntryHandle]
         -> IO BracketResponse
respond' req hs = do
    size <- sum <$> mapM entrySize hs
    let headers' = headers start size
    let okStatus | start == 0 = ok200
                 | otherwise  = partialContent206
    let method = requestMethod req
    let res | method == methodGet  = (okStatus, headers', source hs)
            | method == methodHead = (okStatus, headers', mempty)
            | otherwise            = (status405, [], mempty)
    return res
    where
        start = fromMaybe 0 $ do
            range <- lookup hRange (requestHeaders req)
            parseMaybe rangeParser range

        source :: [FileEntryHandle] -> Source IO (Flush Builder)
        source hs' = (mconcat $ map entrySource hs')
                  =$ consume (fromIntegral start)

        headers :: Integer -> Integer -> ResponseHeaders
        headers from full =
          [ (hContentType, "application/x-tar")
          , (hContentLength, BS.pack $ show (full - from) )
          ] ++ if from == 0
               then [(hContentRange, contentRange from full)]
               else []

        contentRange from full = BS.pack $ "bytes " ++ show from
                                        ++ "-" ++ show (full-1)
                                        ++ "/" ++ show full
        hContentRange = "Content-Range"

-- | Drop the first /n/ bytes and converts the rest to builders.
consume :: Int -> Conduit (Flush BS.ByteString) IO (Flush Builder)
consume n =
    awaitForever $ \input ->
        case input of
            Chunk input' ->
                let remaining = BS.drop n input' in
                if BS.length remaining > 0
                then do yield (Chunk $ fromByteString remaining) 
                        CL.map (fmap fromByteString)
                else consume (n - BS.length input')
            _ -> yield Flush >> consume n

-- | Parse the content of the /Range/ header per
-- <http://tools.ietf.org/html/rfc2616#section-14.35 RFC 2616>.
rangeParser :: Parser Integer
rangeParser = string "bytes=" >> takePosDigit >>= add
    where
        add :: Integer -> Parser Integer
        add x = (takeAddDigit x >>= add) <|> return x

        takeAddDigit :: Integer -> Parser Integer
        takeAddDigit x = (10*x +) <$> takeDigit

        takeDigit :: Parser Integer
        takeDigit = toInt <$> satisfy isDigit

        takePosDigit :: Parser Integer
        takePosDigit = toInt <$> satisfy isPosDigit
        isPosDigit a = isDigit a && a /= '0'

        toInt :: Char -> Integer
        toInt a = toInteger $ ord a - ord '0'

parseMaybe :: Parser a -> BS.ByteString -> Maybe a
parseMaybe p s = either (const Nothing) Just $ parse p "" (decodeUtf8 s)
