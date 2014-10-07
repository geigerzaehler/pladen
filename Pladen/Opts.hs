module Pladen.Opts (
    runOptParse
) where

import Options.Applicative


runOptParse :: IO String
runOptParse = execParser $ info (helper <*> beets) $
    fullDesc
 <> progDesc "Serve up my Beets music library"
    where
        beets = strOption $
              long "beets-db"
           <> short 'b'
           <> metavar "FILENAME"
           <> value "jo"
