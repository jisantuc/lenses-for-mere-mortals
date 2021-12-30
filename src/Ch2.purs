module Ch2 where

import Prelude

import Data.Lens (Lens', lens)
import Data.Map as Map
import Data.Maybe (Maybe(..))

_key :: forall focus. Lens' (Map.Map String focus) (Maybe focus)
_key =
  lens getter setter
  where
  getter whole = Map.lookup "key" whole
  setter whole wrapped = case wrapped of
    Just v -> Map.insert "key" v whole
    Nothing -> Map.delete "key" whole

_atKey
  :: forall key focus
   . Ord key
  => key
  -> Lens' (Map.Map key focus) (Maybe focus)
_atKey key =
  lens getter setter
  where
  getter whole = Map.lookup key whole
  setter whole wrapped = case wrapped of
    Just v -> Map.insert key v whole
    Nothing -> Map.delete key whole