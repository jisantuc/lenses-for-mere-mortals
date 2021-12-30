module Ch1 where

import Prelude

import Data.Lens (_1, _2, lens, set)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (T2, T3, Tuple4, T4, tuple4)

-- roll _1 lens by hand
_first :: forall a b c p. Strong p => p a b -> p (Tuple a c) (Tuple b c)
_first =
  lens getter setter
  where
  getter = fst
  setter (Tuple _ unchanged) new = Tuple new unchanged

_object
  :: forall b r a p
   . Strong p
  => p a b
  -> p
       { object :: a
       | r
       }
       { object :: b
       | r
       }
_object = lens getter setter
  where
  getter = _.object
  setter r new = r { object = new }

fourLong :: Tuple4 Int Int Int Int
fourLong = tuple4 1 2 3 4

set1 :: forall a b ignored. b -> T2 a ignored -> T2 b ignored
set1 new = set _1 new

set2 :: forall a b c ignored. c -> T3 a b ignored -> T3 a c ignored
set2 new = set (_2 <<< _1) new

set3 :: forall a b c d ignored. d -> T4 a b c ignored -> T4 a b d ignored
set3 new = set (_2 <<< _2 <<< _1) new