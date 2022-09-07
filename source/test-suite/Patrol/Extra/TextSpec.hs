module Patrol.Extra.TextSpec where

import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Extra
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Extra.Text" $ do
  Hspec.describe "presence" $ do
    Hspec.it "returns nothing when the text is empty" $ do
      Extra.presence Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "returns just when the text is all spaces" $ do
      let text = Text.singleton ' '
      Extra.presence text `Hspec.shouldBe` Nothing

    Hspec.it "returns just when the text is not empty" $ do
      let text = Text.singleton 'x'
      Extra.presence text `Hspec.shouldBe` Just text
