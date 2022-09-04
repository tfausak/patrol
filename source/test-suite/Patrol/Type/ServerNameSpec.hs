module Patrol.Type.ServerNameSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.ServerName as ServerName
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.ServerName" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      ServerName.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton 'x'
      ServerName.fromText text `Hspec.shouldBe` Just (ServerName.ServerName text)

  Hspec.describe "intoText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton 'x'
      ServerName.intoText (ServerName.ServerName text) `Hspec.shouldBe` text

  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      serverName <- ServerName.fromText $ Text.singleton 'x'
      Aeson.encode serverName `Hspec.shouldBe` Aeson.encode "x"
