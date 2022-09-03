module Patrol.Type.HostSpec where

import qualified Data.Text as Text
import qualified Patrol.Type.Host as Host
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Host" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      Host.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton ' '
      Host.fromText text `Hspec.shouldBe` Just (Host.Host text)

  Hspec.describe "intoText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton ' '
      Host.intoText (Host.Host text) `Hspec.shouldBe` text
