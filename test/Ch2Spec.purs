module Test.Ch2Spec where

import Ch2 (_atKey)
import Data.Eq (class Eq)
import Data.Lens (Lens', setJust, view)
import Data.Lens.At (at)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Show (class Show)
import Data.Unit (Unit)
import Prelude (class Ord, discard, ($))
import Test.QuickCheck (class Arbitrary, Result, (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Ch2 spec" do
    describe "Lens equivalences" do
      it "Views equivalently" $ quickCheck
        ( \(k :: String) (v :: Int) ->
            opticViewEquivalenceTest k v (at k) (_atKey k)
        )
      it "Sets equivalently" $ quickCheck
        ( \(k :: String) (v :: Int) ->
            opticSetEquivalenceTest k v at _atKey
        )
    describe "Operations" do
      it "Sets successfully" $ setJust (_atKey "foo") 3 Map.empty `shouldEqual` Map.singleton "foo" 3

opticViewEquivalenceTest
  :: forall k v
   . Eq v
  => Show v
  => Arbitrary k
  => Arbitrary v
  => Ord k
  => k
  -> v
  -> Lens' (Map.Map k v) (Maybe v)
  -> Lens' (Map.Map k v) (Maybe v)
  -> Result
opticViewEquivalenceTest key value l1 l2 =
  let
    input = Map.singleton key value
  in
    view l1 input === view l2 input

opticSetEquivalenceTest
  :: forall k v
   . Eq v
  => Show v
  => Show k
  => Arbitrary k
  => Arbitrary v
  => Ord k
  => k
  -> v
  -> (k -> Lens' (Map.Map k v) (Maybe v))
  -> (k -> Lens' (Map.Map k v) (Maybe v))
  -> Result
opticSetEquivalenceTest k v l1 l2 =
  setJust (l1 k) v Map.empty === setJust (l2 k) v Map.empty