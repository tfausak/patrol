module Patrol.Type.ModuleVersionSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.ModuleVersion as ModuleVersion
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.ModuleVersion" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      ModuleVersion.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton 'x'
      ModuleVersion.fromText text `Hspec.shouldBe` Just (ModuleVersion.ModuleVersion text)

  Hspec.describe "intoText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton 'x'
      ModuleVersion.intoText (ModuleVersion.ModuleVersion text) `Hspec.shouldBe` text

  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      moduleVersion <- ModuleVersion.fromText $ Text.singleton 'x'
      Aeson.encode moduleVersion `Hspec.shouldBe` Aeson.encode "x"
