{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.EventTypeSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Patrol.Type.EventType as EventType
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.EventType" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works for Csp" $ do
      Aeson.toJSON EventType.Csp `Hspec.shouldBe` [Aeson.aesonQQ| "csp" |]

    Hspec.it "works for Default" $ do
      Aeson.toJSON EventType.Default `Hspec.shouldBe` [Aeson.aesonQQ| "default" |]

    Hspec.it "works for Error" $ do
      Aeson.toJSON EventType.Error `Hspec.shouldBe` [Aeson.aesonQQ| "error" |]

    Hspec.it "works for Expectct" $ do
      Aeson.toJSON EventType.Expectct `Hspec.shouldBe` [Aeson.aesonQQ| "expectct" |]

    Hspec.it "works for Expectstaple" $ do
      Aeson.toJSON EventType.Expectstaple `Hspec.shouldBe` [Aeson.aesonQQ| "expectstaple" |]

    Hspec.it "works for Hpkp" $ do
      Aeson.toJSON EventType.Hpkp `Hspec.shouldBe` [Aeson.aesonQQ| "hpkp" |]

    Hspec.it "works for Transaction" $ do
      Aeson.toJSON EventType.Transaction `Hspec.shouldBe` [Aeson.aesonQQ| "transaction" |]
