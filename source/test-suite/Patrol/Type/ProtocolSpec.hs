module Patrol.Type.ProtocolSpec where

import qualified Data.Text as Text
import qualified Patrol.Type.Protocol as Protocol
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Protocol" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      Protocol.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton ' '
      Protocol.fromText text `Hspec.shouldBe` Just (Protocol.Protocol text)

  Hspec.describe "toText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton ' '
      Protocol.toText (Protocol.Protocol text) `Hspec.shouldBe` text
