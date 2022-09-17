{-# LANGUAGE QuasiQuotes #-}

module Patrol.Extra.AesonSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Patrol.Extra.Aeson as Extra
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Extra.Aeson" $ do
  Hspec.describe "intoObject" $ do
    Hspec.it "works with an empty object" $ do
      Extra.intoObject [] `Hspec.shouldBe` [Aeson.aesonQQ| {} |]

    Hspec.it "strips null" $ do
      Extra.intoObject [Extra.pair "k" Aeson.Null] `Hspec.shouldBe` [Aeson.aesonQQ| {} |]

    Hspec.it "does not strip false" $ do
      Extra.intoObject [Extra.pair "k" False] `Hspec.shouldBe` [Aeson.aesonQQ| { "k": false } |]

    Hspec.it "does not strip zero" $ do
      Extra.intoObject [Extra.pair "k" (0 :: Int)] `Hspec.shouldBe` [Aeson.aesonQQ| { "k": 0 } |]

    Hspec.it "strips empty string" $ do
      Extra.intoObject [Extra.pair "k" ""] `Hspec.shouldBe` [Aeson.aesonQQ| {} |]

    Hspec.it "strips empty array" $ do
      Extra.intoObject [Extra.pair "k" (mempty :: [Int])] `Hspec.shouldBe` [Aeson.aesonQQ| {} |]

    Hspec.it "strips empty object" $ do
      Extra.intoObject [Extra.pair "k" (mempty :: Map.Map String Int)] `Hspec.shouldBe` [Aeson.aesonQQ| {} |]

  Hspec.describe "isEmpty" $ do
    Hspec.it "is true for null" $ do
      Aeson.Null `Hspec.shouldSatisfy` Extra.isEmpty

    Hspec.it "is false for false" $ do
      Aeson.Bool False `Hspec.shouldNotSatisfy` Extra.isEmpty

    Hspec.it "is false for zero" $ do
      Aeson.Number 0 `Hspec.shouldNotSatisfy` Extra.isEmpty

    Hspec.it "is true for an empty string" $ do
      Aeson.String mempty `Hspec.shouldSatisfy` Extra.isEmpty

    Hspec.it "is true for an empty array" $ do
      Aeson.Array mempty `Hspec.shouldSatisfy` Extra.isEmpty

    Hspec.it "is true for an empty object" $ do
      Aeson.Object mempty `Hspec.shouldSatisfy` Extra.isEmpty

  Hspec.describe "pair" $ do
    Hspec.it "works" $ do
      Extra.pair "key" False `Hspec.shouldBe` (Key.fromString "key", Aeson.Bool False)
