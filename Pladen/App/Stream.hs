{-# LANGUAGE OverloadedStrings #-}

module Pladen.App.Stream
    (
      streamTrack
    ) where

import Control.Monad
import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Text.ShellEscape as E
import Text.Printf (printf)

import System.IO
import System.Directory (doesFileExist)
import qualified System.Process as P

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Blaze.ByteString.Builder (fromByteString, Builder)

import Network.Mime
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai

import Pladen.Beets.Persist
import Pladen.App.Controller
import Pladen.App.Environment
import System.FilePath


-- | Stream the audio data as ogg or mp3.
streamTrack :: Track -> Controller
streamTrack track = do
    c <- getConfig
    let path = musicDir c </> makeRelative (beetsDir c) (trackPath track)
    guard =<< liftIO (doesFileExist path)
    respond =<< liftIO (response path)
    where
        response p | trackFormat track == "OGG"
                   = return $ fileResponse mimeOgg p
                   | trackFormat track == "MP3"
                   = return $ fileResponse mimeMp3 p
                   | otherwise
                   = gstreamer p duration
        fileResponse mime path =
            responseFile ok200 (mediaHeaders mime duration) path Nothing
        duration = trackLength track

-- | Stream the output of gstreamer as the reponse
gstreamer :: FilePath
          -> Double
          -> IO Response
gstreamer path duration = responseSourceBracket setup teardown send
    where
        -- | Spawn Gstreamer 
        setup :: IO (Maybe Handle, P.ProcessHandle)
        setup = do
            (mbin, mbout, _, ph) <- P.createProcess $ gstreamerCmd path
            maybe (return ()) hClose mbin
            return (mbout, ph)

        -- | Kill Gstreamer process and wait for it to finish
        teardown :: (Maybe Handle, P.ProcessHandle) -> IO ()
        teardown (mbout, ph) = do
            maybe (return ()) hClose mbout
            P.terminateProcess ph
            _ <- P.waitForProcess ph
            return ()

        send (mbout, _) = ensureHandle mbout
                (status500, [], mempty) $
                \h -> ( status200
                      , mediaHeaders mimeOgg duration
                      , sourceHandle h)


mediaHeaders :: MimeType -> Double -> ResponseHeaders
mediaHeaders mime duration = 
    let duration' = BS.pack $ printf "%.1f" duration in
    [ (hContentType, mime)
    , ("X-Content-Duration", duration')
    ]


mimeOgg :: MimeType
mimeOgg = "audio/ogg"

mimeMp3 :: MimeType
mimeMp3 = "audio/mpeg"


-- | Create a gstreamer process the converts the file to OGG/Vorbis and
-- sends the result to stdout.
gstreamerCmd :: FilePath -> P.CreateProcess
gstreamerCmd path = (P.shell $ gstreamerCmdString path)
                      { P.std_in = P.CreatePipe
                      , P.std_out = P.CreatePipe
                      }

-- | Build the command line for gstreamer to convert a file to
-- OGG/Vorbis and send it to stdout.
gstreamerCmdString :: FilePath -> String
gstreamerCmdString path = "gst-launch-0.10 --quiet"
                       ++ " filesrc location=" ++ shellEscape path
                       ++ " ! decodebin2 ! audioconvert ! vorbisenc ! oggmux ! fdsink"


shellEscape :: String -> String
shellEscape = BS.unpack . E.bytes . E.sh . BS.pack



-- | Run a function if the handle provides data.
ensureHandle :: Maybe Handle
             -> a              -- ^ Return this if handle is 'Nothing' or EOF
             -> (Handle -> a)
             -> IO a
ensureHandle mbHandle fallback go =
    maybe (return fallback) checkEOF mbHandle
    where checkEOF h = do
            eof <- hIsEOF h
            return $ if eof
                     then fallback
                     else go h

-- | Convert a 'Handle' into a 'Builder' source.
sourceHandle :: Handle -> Source IO (Flush Builder)
sourceHandle h = CB.sourceHandle h =$ CL.map (Chunk . fromByteString)
