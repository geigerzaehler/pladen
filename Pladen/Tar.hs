module Pladen.Tar
    (

    -- * Archive handles
      Archive
    , FileEntryHandle
    , openEntry
    , openEntries
    , closeEntry

    -- * Stream archive
    , entrySource
    , entrySize
    ) where


import Data.Monoid ((<>))
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe

import System.IO ( openFile, hFileSize, IOMode(..), hClose, Handle )
import System.FilePath ((</>), splitPath)
import System.Directory (doesFileExist)
import Data.Time.Clock.POSIX (getPOSIXTime)

import           Data.Conduit
import qualified Data.Conduit.List   as CL
import qualified Data.Conduit.Binary as CB

import           Data.ByteString.Builder
import qualified Data.ByteString.UTF8 as UTF8
import           Data.ByteString.Lazy (toStrict)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Numeric (showOct)
import           Data.Word (Word32)


data FileEntry = FileEntry TarPath FilePath
    deriving (Show, Eq)
data FileEntryHandle = FileEntryHandle TarPath Handle
    deriving (Show, Eq)

data Header = Header {
    hTarPath     :: TarPath
  , hSize        :: Integer
  , hPermissions :: Word32
  , hModTime     :: Integer
  , hContentType :: ContentType
  }

data ContentType = FileContent
                 | DirectoryContent

defPerm :: Word32
defPerm = 0o664

type Archive = [(FilePath, FilePath)]

-- | Create an archive entry and open file handlers. The action
-- may fail if the file does not exist or the path in the archive
-- is too long. Be sure to close file handlers with 'closeEntry'.
openEntry :: FilePath  -- ^ Local file to put into archive
          -> FilePath  -- ^ Location in archive
          -> IO (Maybe FileEntryHandle)
openEntry src tar = runMaybeT $ do
    tp <- MaybeT $ return $ tarPath tar
    exists <- liftIO $ doesFileExist src
    guard exists
    liftIO $ openEntry' (FileEntry tp src)


-- TODO handle exceptions
openEntries :: Archive -> IO (Maybe [FileEntryHandle])
openEntries a = open [] a
    where
        open :: [FileEntryHandle] -> Archive -> IO (Maybe [FileEntryHandle])
        open es [] = return $ Just es
        open es ((f,t):as) = do
            maybeEntry <- openEntry f t
            case maybeEntry of
                Just e  -> open (e:es) as
                Nothing -> do mapM_ closeEntry es
                              return Nothing

openEntry' :: FileEntry -> IO FileEntryHandle
openEntry' (FileEntry t p) = do
    h <- openFile p ReadMode
    return $ FileEntryHandle t h


closeEntry :: FileEntryHandle -> IO ()
closeEntry (FileEntryHandle _ h)= hClose h


entrySource :: FileEntryHandle -> Source IO (Flush BS.ByteString)
entrySource (FileEntryHandle p h) = do
    size <- fromIntegral <$> liftIO (hFileSize h)
    time <- liftIO getPOSIXTime

    let header = Header p size defPerm (round time) FileContent
    yield $ Chunk $ toByteString $ buildHeader header
    yield Flush

    CB.sourceHandle h =$ CL.map Chunk

    let paddingSize = fromIntegral $ negate (hSize header) `mod` 512
    yield $ Chunk $ BS.replicate (paddingSize) 0


-- | Get the number of bytes that will be streamed by 'entrySource'.
entrySize :: FileEntryHandle -> IO Integer
entrySize (FileEntryHandle _ h) = do
    fsize <- hFileSize h
    return $ 512 -- header size
             + fsize
             + (negate fsize `mod` 512)


-----------------------------------------------------------
-- Paths in Archive
-----------------------------------------------------------

data TarPath = TarPath FilePath -- path name, 100 characters max.
                       FilePath -- path prefix, 155 characters max.
     deriving (Show, Eq)

tarPath :: FilePath -> Maybe TarPath
tarPath p = foldM prepend (TarPath "" "") (reverse $ splitPath p)
    where
        prepend :: TarPath -> String -> Maybe TarPath
        prepend (TarPath a b) x = 
            if a == "" && length (x </> b) <= 100
            then Just $ TarPath a (x </> b)
            else if length (x </> a) <= 155
            then Just $ TarPath (x </> a) b
            else Nothing


hTarPathName :: Header -> FilePath
hTarPathName e = let TarPath _ name = hTarPath e
                 in name
hTarPathPrefix :: Header -> FilePath
hTarPathPrefix e = let TarPath prefix _ = hTarPath e
                   in prefix


-----------------------------------------------------------
-- Building the Header
-----------------------------------------------------------

buildHeader :: Header -> Builder
buildHeader e = pre
        <> paddedOct 7 chk <> fill 1 ' '
        <> post
    where
        pre  = preChecksumHeader e
        post = postChecksumHeader e
        chk = BS.foldl' (\x y -> x + fromEnum y) 0 dummy
        dummy = toByteString $ pre <> fill 8 ' ' <> post
        

preChecksumHeader :: Header -> Builder
preChecksumHeader h =
        paddedString 100 (hTarPathName h)
     <> paddedOct      8 (hPermissions h)
     <> paddedOct      8 (0 :: Int)  -- owner uid
     <> paddedOct      8 (0 :: Int)  -- owner gid
     <> paddedOct     12 (hSize h)
     <> paddedOct     12 (hModTime h)

postChecksumHeader :: Header -> Builder
postChecksumHeader h =
    char8            (typeFlag $ hContentType h)
 <> fillNul      100             -- link target
 <> string8          "ustar\NUL" -- format magic
 <> string8          "00"        -- version
 <> fillNul       32             -- owner name
 <> fillNul       32             -- group name
 <> paddedOct      8 (0 :: Int)  -- device major
 <> paddedOct      8 (0 :: Int)  -- device minor
 <> paddedString 155 (hTarPathPrefix h) 
 <> fillNul       12
    where
        typeFlag FileContent      = '0'
        typeFlag DirectoryContent = '5'


-----------------------------------------------------------
-- Builder Helpers
-----------------------------------------------------------

fill :: Int -> Char -> Builder
fill width c = string8 $ replicate width c

fillNul :: Int -> Builder
fillNul w = fill w '\NUL'

paddedOct :: (Integral a, Show a) => Int -> a -> Builder
paddedOct width x =
  let octStr = take (width-1) $ showOct x ""
      fillWidth = width - length octStr - 1
   in 
      fill fillWidth '0'
   <> string8 octStr
   <> char8   '\NUL'

paddedString :: Int -> String -> Builder
paddedString width c = byteString bs <> pad
    where
        bs =  BS.take (width-1) $ UTF8.fromString c
        pad = fillNul (width - BS.length bs)

toByteString :: Builder -> ByteString
toByteString = toStrict . toLazyByteString
