module Patrol.Extra.AesonSpec where

import qualified Data.Aeson as Aeson
import qualified Patrol.Extra.Aeson as Extra
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Extra.Aeson" $ do
  Hspec.describe "isEmpty" $ do
    Hspec.it "is true for null" $ do
      Extra.isEmpty Aeson.Null `Hspec.shouldBe` True

    Hspec.it "is false for false" $ do
      Extra.isEmpty (Aeson.Bool False) `Hspec.shouldBe` False

    Hspec.it "is false for zero" $ do
      Extra.isEmpty (Aeson.Number 0) `Hspec.shouldBe` False

    Hspec.it "is false for an empty string" $ do
      Extra.isEmpty (Aeson.String mempty) `Hspec.shouldBe` False

    Hspec.it "is true for an empty array" $ do
      Extra.isEmpty (Aeson.Array mempty) `Hspec.shouldBe` True

    Hspec.it "is true for an empty object" $ do
      Extra.isEmpty (Aeson.Object mempty) `Hspec.shouldBe` True
