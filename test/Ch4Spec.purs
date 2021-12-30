module Test.Ch4Spec where

import Control.Category ((<<<))
import Data.Lens (element, over, preview, traversed, view, _1)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Unit (Unit)
import Prelude (($), discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Tuple (Tuple(..))

spec :: Spec Unit
spec = do
  describe "Ch4 spec" do
    describe "Prediction" do
      it "add additives without map" $ (view traversed <<< over traversed Additive) [ 1, 2, 3 ] `shouldEqual` Additive 6
      it "gets the second element of an array when present" $ preview (element 1 traversed) [ 1, 2, 3 ] `shouldEqual` Just 2
    describe "Composition" do
      it "Converts [[\"1\"], [\"2\", \"3\"]] to \"123\"" $ view (traversed <<< traversed) [ [ "1" ], [ "2", "3" ] ] `shouldEqual` "123"
      it "Gets the first tuple element from each of several tuples" $ preview (traversed <<< _1) [ Tuple 1 2, Tuple 3 4 ] `shouldEqual` Just 1