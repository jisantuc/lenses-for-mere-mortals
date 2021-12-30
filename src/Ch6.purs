module Ch6 where

import Prelude

import Data.Int (fromString)
import Data.Lens (Prism', only, prism')
import Data.Maybe (Maybe(..), isJust)

_intSource :: Prism' String String
_intSource = prism' identity focuser
  where
  focuser s = if (isJust $ fromString s) then Just s else Nothing

_toInt :: Prism' String Int
_toInt = prism' show fromString

_word :: String -> Prism' String Unit
_word s = only s