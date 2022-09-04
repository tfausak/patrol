module Patrol.Extra.TextSpec where

import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Extra.Text" $ do
  Hspec.describe "presence" $ do
    Hspec.it "returns nothing when the text is empty" $ do
      Text.presence Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "returns just when the text is all spaces" $ do
      let text = Text.singleton ' '
      Text.presence text `Hspec.shouldBe` Nothing

    Hspec.it "returns just when the text is not empty" $ do
      let text = Text.singleton 'x'
      Text.presence text `Hspec.shouldBe` Just text
