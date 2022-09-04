module Patrol.Type.TagKeySpec where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Patrol.Type.TagKey as TagKey
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.TagKey" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      TagKey.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton 'x'
      TagKey.fromText text `Hspec.shouldBe` Just (TagKey.TagKey text)

  Hspec.describe "intoText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton 'x'
      TagKey.intoText (TagKey.TagKey text) `Hspec.shouldBe` text

  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      tagKey <- TagKey.fromText $ Text.singleton 'x'
      Aeson.encode tagKey `Hspec.shouldBe` Aeson.encode "x"

  Hspec.describe "ToJSONKey" $ do
    Hspec.it "works" $ do
      tagKey <- TagKey.fromText $ Text.singleton 'x'
      let tags = Map.singleton tagKey False
      Aeson.encode tags `Hspec.shouldBe` LazyByteString.fromStrict (Text.encodeUtf8 $ Text.pack "{\"x\":false}")
