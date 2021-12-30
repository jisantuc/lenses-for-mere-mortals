module Test.Ch1Spec where

import Prelude

import Ch1 (_first, _object, fourLong, set1, set2, set3)
import Data.Lens (_1, _2, over, set, view)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple4)
import Effect.Class (liftEffect)
import Test.QuickCheck (Result, quickCheck, (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type TupleLens c = forall (a ∷ Type) (b ∷ Type) (p ∷ Type -> Type -> Type). Strong p ⇒ p a b → p (Tuple a c) (Tuple b c)

spec :: Spec Unit
spec = do
  describe "Lens equivalences" do
    describe "First element focuse" do
      it "focuses first element" $ liftEffect $ quickCheck (\(x :: Tuple String Int) -> opticViewEquivalenceTest x _1 _first)
      it "modifies first element" $ liftEffect $ quickCheck (\(x :: Tuple String Int) -> opticOverEquivalenceTest x _1 _first)
  describe "Record update" do
    it "replaces the object" $ (over _object (const 3) exampleRecord).object `shouldEqual` 3
  describe "Composition" do
    it "Stringifies extracts object from the second element" $
      let
        _adHoc = _2 <<< _object
        replaced = set _2 (view _adHoc tupleRecord) tupleRecord
      in
        replaced `shouldEqual` Tuple "example" "Dawn"
    describe "Nested tuple exercises" do
      it "Replaces the first element" $ set1 5 fourLong `shouldEqual` tuple4 5 2 3 4
      it "Replaces the second element" $ set2 "abcde" fourLong `shouldEqual` tuple4 1 "abcde" 3 4
      it "Replaces the third element" $ set3 "foo" fourLong `shouldEqual` tuple4 1 2 "foo" 4

opticViewEquivalenceTest :: forall a b. Eq a => Show a => Tuple a b -> TupleLens b -> TupleLens b -> Result
opticViewEquivalenceTest input l1 l2 = view l1 input === view l2 input

opticOverEquivalenceTest :: forall a b. Eq b => Show b => Tuple a b -> TupleLens b -> TupleLens b -> (a -> String) -> Result
opticOverEquivalenceTest input l1 l2 f = over l1 f input === over l2 f input

type RecordType =
  { action :: String
  , count :: Int
  , object :: String
  , subject :: String
  }

exampleRecord :: RecordType
exampleRecord =
  { action: "cafune"
  , count: 0
  , object: "Dawn"
  , subject: "Briant"
  }

tupleRecord :: Tuple String RecordType
tupleRecord = Tuple "example" exampleRecord