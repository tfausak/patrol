module Patrol.Type.LoggerSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.Logger as Logger
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Logger" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      Logger.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton 'x'
      Logger.fromText text `Hspec.shouldBe` Just (Logger.Logger text)

  Hspec.describe "intoText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton 'x'
      Logger.intoText (Logger.Logger text) `Hspec.shouldBe` text

  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      logger <- Logger.fromText $ Text.singleton 'x'
      Aeson.encode logger `Hspec.shouldBe` Aeson.encode "x"
