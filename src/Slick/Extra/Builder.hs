{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Slick.Extra.Builder
  ( requireEntity
  , requireFile
  , EntityFilePath(..)
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.List                  as L (intersperse, sortBy, (\\))
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                  as T
import           Development.Shake          as Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           GHC.Generics               hiding (Meta)
import           Slick
import           Slick.Pandoc
import           System.Console.GetOpt
import           System.Directory.Extra     (listFilesRecursive, removeFile)
import           System.IO.Extra            as IO
import           Text.Pandoc
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Shared

--------------------------------------------------------------------------------

-- | Generalized version of filepath
newtype EntityFilePath a =
  EntityFilePath String
   deriving (Show, Eq,  Generic, Hashable, Binary, NFData)

-- | Find specific file and tell Shake to build the corresponding html page
requireFile :: String -> Action ()
requireFile path = do
  need [srcToDest "public" path -<.> "html"]

-- | Find all speified entity source files and
--   tell Shake to build corresponding html pages
requireEntity :: String      -- ^ top dir
              -> String      -- ^ directory were specified objects live
              -> String      -- ^ file pattern
              -> Action ()
requireEntity tdir dir patrn = do
  entityNames <- fileNames tdir dir patrn
  need ((\p -> srcToDest "public" p -<.> "html") <$> entityNames)

-- | Generalized Entity Loader, wrapper in
--   own function to respolve EntityFilePath properly
loadEntity :: FromJSON b
           => ReaderOptions      -- ^ Pandoc reader options to specify extensions or other functionality
           -> WriterOptions      -- ^ Pandoc writer options to modify output
           -> FilePath           -- ^ Content directory
           -> EntityFilePath a   -- ^ Path to entity ready for conversion, usually text file
           -> Action b
loadEntity rops wops dir (EntityFilePath path) =
  subloadEntity rops wops dir path

-- | Helper function for generalized `loadEntity`
--   provides conversion from source to output file format
subloadEntity :: FromJSON b
              => ReaderOptions  -- ^ Pandoc reader options to specify extensions or other functionality
              -> WriterOptions  -- ^ Pandoc writer options to modify output
              -> FilePath       -- ^ Path to content directoryx
              -> FilePath       -- ^ Path to the file ready for conversion
              -> Action b
subloadEntity rops wops dir entityPath = do
  let srcPath = destToSrc dir entityPath -<.> "md"
  entityData <- Shake.readFile' srcPath >>= markdownToHTML rops wops . T.pack
  let entityURL = T.pack  . srcToURL $ entityPath
      withURL = _Object . at "url"     ?~ String entityURL
      withSrc = _Object . at "srcPath" ?~ String (T.pack srcPath)
  convert . withSrc . withURL $ entityData


--------------------------------------------------------------------------------
-- FilePaths loaders

-- | Get names of the files from `site` subdirectory, embeds into existing path structure
fileNames :: String            -- ^ top dir where content lives, like "site/"
          -> String            -- ^ subdir path
          -> String            -- ^ pattern for file "//*.md"
          -> Action [FilePath]
fileNames tdir dir patrn  =
  getDirectoryFiles "." [tdir ++ dir ++ patrn]

-- | Get names of the files from provided directory pattern, full path
fileNamesByPattern :: String -> Action [FilePath]
fileNamesByPattern pattern' =
  getDirectoryFiles "." [pattern']
