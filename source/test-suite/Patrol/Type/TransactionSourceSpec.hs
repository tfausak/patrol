{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.TransactionSourceSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Patrol.Type.TransactionSource as TransactionSource
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.TransactionSource" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works for Component" $ do
      Aeson.toJSON TransactionSource.Component `Hspec.shouldBe` [Aeson.aesonQQ| "component" |]

    Hspec.it "works for Custom" $ do
      Aeson.toJSON TransactionSource.Custom `Hspec.shouldBe` [Aeson.aesonQQ| "custom" |]

    Hspec.it "works for Route" $ do
      Aeson.toJSON TransactionSource.Route `Hspec.shouldBe` [Aeson.aesonQQ| "route" |]

    Hspec.it "works for Task" $ do
      Aeson.toJSON TransactionSource.Task `Hspec.shouldBe` [Aeson.aesonQQ| "task" |]

    Hspec.it "works for Url" $ do
      Aeson.toJSON TransactionSource.Url `Hspec.shouldBe` [Aeson.aesonQQ| "url" |]

    Hspec.it "works for Unknown" $ do
      Aeson.toJSON TransactionSource.Unknown `Hspec.shouldBe` [Aeson.aesonQQ| "unknown" |]

    Hspec.it "works for View" $ do
      Aeson.toJSON TransactionSource.View `Hspec.shouldBe` [Aeson.aesonQQ| "view" |]
