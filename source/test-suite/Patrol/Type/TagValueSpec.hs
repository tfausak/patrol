module Patrol.Type.TagValueSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.TagValue as TagValue
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.TagValue" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      TagValue.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton 'x'
      TagValue.fromText text `Hspec.shouldBe` Just (TagValue.TagValue text)

  Hspec.describe "intoText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton 'x'
      TagValue.intoText (TagValue.TagValue text) `Hspec.shouldBe` text

  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      tagValue <- TagValue.fromText $ Text.singleton 'x'
      Aeson.encode tagValue `Hspec.shouldBe` Aeson.encode "x"
