module Patrol.Type.PublicKeySpec where

import qualified Data.Text as Text
import qualified Patrol.Type.PublicKey as PublicKey
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.PublicKey" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      PublicKey.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton ' '
      PublicKey.fromText text `Hspec.shouldBe` Just (PublicKey.PublicKey text)

  Hspec.describe "intoText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton ' '
      PublicKey.intoText (PublicKey.PublicKey text) `Hspec.shouldBe` text
