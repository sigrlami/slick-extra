{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Slick.Extra.Builder
  ( srcToURL
  , destToSrc
  , srcToDest
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

-- | convert 'build' filepaths into source file filepaths
destToSrc :: FilePath -> FilePath
destToSrc p = "site" </> dropDirectory1 p

-- | convert source filepaths into build filepaths
srcToDest :: FilePath -> FilePath
srcToDest p = "dist" </> dropDirectory1 p

-- | convert a source file path into a URL
srcToURL :: FilePath -> String
srcToURL = ("/" ++) . dropDirectory1 . (-<.> ".html")

-- | Generalized Entity Loader, wrapper in
--   own function to respolve EntityFilePath properly
loadEntity :: FromJSON b
           => ReaderOptions      -- ^ Pandoc reader options to specify extensions or other functionality
           -> WriterOptions      -- ^ Pandoc writer options to modify output
           -> EntityFilePath a   -- ^ Path to entity ready for conversion, usually text file
           -> Action b
loadEntity rops wops (EntityFilePath path) =
  subloadEntity rops wops path

-- | Helper functiona for generalized `loadEntity`
--   provides conversion from source to output file format
subloadEntity :: FromJSON b
              => ReaderOptions  -- ^ Pandoc reader options to specify extensions or other functionality
              -> WriterOptions  -- ^ Pandoc writer options to modify output
              -> FilePath       -- ^ Path to the file ready for conversion
              -> Action b
subloadEntity rops wops entityPath = do
  let srcPath = destToSrc entityPath -<.> "md"
  entityData <- Shake.readFile' srcPath >>= markdownToHTML rops wops . T.pack
  let entityURL = T.pack  . srcToURL $ entityPath
      withURL = _Object . at "url"     ?~ String entityURL
      withSrc = _Object . at "srcPath" ?~ String (T.pack srcPath)
  convert . withSrc . withURL $ entityData
