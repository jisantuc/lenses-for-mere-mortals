module Test.Ch6Spec where

import Ch6 (_intSource, _toInt, _word)
import Control.Category ((>>>))
import Data.Eq (class Eq)
import Data.Lens (Prism', preview, review)
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.Unit (Unit, unit)
import Prelude (discard)
import Test.QuickCheck (Result, (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec = describe "Ch6 Spec" do
  describe "prisms strings to number strings" do
    it "gets expected values" do
      preview _intSource "134" `shouldEqual` Just "134"
      preview _intSource "a35" `shouldEqual` Nothing
    it "follows laws (doesn't actually -- see comment here)" do
      -- it doesn't actually, since the constructor works on any string.
      -- i'm not sure if I've done something wrong or if this just isn't a lawful prism
      -- examples say that this _does_ follow laws, but the laws in e.g.:
      -- https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references#Laws
      -- don't allow partiality in the reviewPreviewCheck
      expectError (quickCheck (\s -> reviewPreviewCheck s _intSource))
      quickCheck (\s -> previewReviewCheck s _intSource)
  describe "prisms strings to numbers" do
    it "produces numbers from number strings" do
      preview _toInt "134" `shouldEqual` Just 134
      preview _toInt "a35" `shouldEqual` Nothing
    -- ok this one _is_ lawful, because int show gets an int I can read,
    -- while arbitrary strings don't guarantee that.
    it "follows laws" do
      quickCheck (\s -> reviewPreviewCheck s _toInt)
      quickCheck (\s -> previewReviewCheck s _toInt)
  describe "matches a special word" do
    it "produces correct results" do
      preview (_word "hello") "hello" `shouldEqual` Just unit
      preview (_word "hello") "goodbye" `shouldEqual` Nothing
    it "follows lens laws" do
      quickCheck (\u -> reviewPreviewCheck u (_word "hello"))
      quickCheck (\s -> previewReviewCheck s (_word s))

reviewPreviewCheck :: forall s a. Eq a => Show a => a -> Prism' s a -> Result
reviewPreviewCheck input pr = (review pr >>> preview pr) input === Just input

previewReviewCheck :: forall s a. Eq s => Show s => s -> Prism' s a -> Result
previewReviewCheck input pr =
  case preview pr input of
    Just o -> review pr o === input
    Nothing -> unit === unit