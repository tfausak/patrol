module Patrol.Type.ModuleNameSpec where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Patrol.Type.ModuleName as ModuleName
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.ModuleName" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      ModuleName.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton 'x'
      ModuleName.fromText text `Hspec.shouldBe` Just (ModuleName.ModuleName text)

  Hspec.describe "intoText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton 'x'
      ModuleName.intoText (ModuleName.ModuleName text) `Hspec.shouldBe` text

  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      moduleName <- ModuleName.fromText $ Text.singleton 'x'
      Aeson.encode moduleName `Hspec.shouldBe` Aeson.encode "x"

  Hspec.describe "ToJSONKey" $ do
    Hspec.it "works" $ do
      moduleName <- ModuleName.fromText $ Text.singleton 'x'
      let tags = Map.singleton moduleName False
      Aeson.encode tags `Hspec.shouldBe` LazyByteString.fromStrict (Text.encodeUtf8 $ Text.pack "{\"x\":false}")
