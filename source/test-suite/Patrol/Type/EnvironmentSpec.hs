module Patrol.Type.EnvironmentSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.Environment as Environment
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Environment" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      Environment.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton 'x'
      Environment.fromText text `Hspec.shouldBe` Just (Environment.Environment text)

  Hspec.describe "intoText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton 'x'
      Environment.intoText (Environment.Environment text) `Hspec.shouldBe` text

  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      environment <- Environment.fromText $ Text.singleton 'x'
      Aeson.encode environment `Hspec.shouldBe` Aeson.encode "x"

  Hspec.describe "production" $ do
    Hspec.it "is correct" $ do
      production <- Environment.fromText $ Text.pack "production"
      Environment.production `Hspec.shouldBe` production
