module Slick.Extra.TH (dropPrefix) where

import           Data.Char (toLower)

--------------------------------------------------------------------------------

dropPrefix :: Int -> String -> String
dropPrefix n str =
  toLower c : later
    where
      (c:later) = drop n str
