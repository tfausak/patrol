module Patrol.Type.SecretKeySpec where

import qualified Data.Text as Text
import qualified Patrol.Type.SecretKey as SecretKey
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.SecretKey" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      SecretKey.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton ' '
      SecretKey.fromText text `Hspec.shouldBe` Just (SecretKey.SecretKey text)

  Hspec.describe "toText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton ' '
      SecretKey.toText (SecretKey.SecretKey text) `Hspec.shouldBe` text
