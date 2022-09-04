module Patrol.Type.DistSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.Dist as Dist
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Dist" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      Dist.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton 'x'
      Dist.fromText text `Hspec.shouldBe` Just (Dist.Dist text)

  Hspec.describe "intoText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton 'x'
      Dist.intoText (Dist.Dist text) `Hspec.shouldBe` text

  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      dist <- Dist.fromText $ Text.singleton 'x'
      Aeson.encode dist `Hspec.shouldBe` Aeson.encode "x"
