module Plugin.GhcTags.CTags where

import Data.Version

import Paths_ghc_tags_plugin


headers :: [(String, String)]
headers =
    [ ("TAG_FILE_FORMAT",     "2") -- format 1 does not append ';"' to lines
    , ("TAG_FILE_SORTED",     "1") -- allows for  binary search
    , ("TAG_FILE_ENCODING",   "utf-8")
    , ("TAG_PROGRAM_AUTHOR",  "Marcin Szamotulski")
    , ("TAG_PROGRAM_NAME",    "ghc-tags-plugin")
    , ("TAG_PROGRAM_URL",     "https://hackage.haskell.org/package/ghc-tags-plugin")
    , ("TAG_PROGRAM_VERSION", (showVersion version))
    ]
