module Patrol.Type.TransactionSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.Transaction as Transaction
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Transaction" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      Transaction.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton 'x'
      Transaction.fromText text `Hspec.shouldBe` Just (Transaction.Transaction text)

  Hspec.describe "intoText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton 'x'
      Transaction.intoText (Transaction.Transaction text) `Hspec.shouldBe` text

  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      transaction <- Transaction.fromText $ Text.singleton 'x'
      Aeson.encode transaction `Hspec.shouldBe` Aeson.encode "x"
