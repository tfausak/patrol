module Patrol.Type.NonEmptyTextSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.NonEmptyText as NonEmptyText
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.NonEmptyText" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      NonEmptyText.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "fails when the text is all spaces" $ do
      let text = Text.singleton ' '
      NonEmptyText.fromText text `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton 'x'
      NonEmptyText.fromText text `Hspec.shouldBe` Just (NonEmptyText.NonEmptyText text)

  Hspec.describe "intoText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton 'x'
      nonEmptyText <- NonEmptyText.fromText text
      NonEmptyText.intoText nonEmptyText `Hspec.shouldBe` text

  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      nonEmptyText <- NonEmptyText.fromText $ Text.singleton 'x'
      Aeson.encode nonEmptyText `Hspec.shouldBe` Aeson.encode "x"
