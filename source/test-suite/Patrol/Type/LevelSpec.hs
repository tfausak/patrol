{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.LevelSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Patrol.Type.Level as Level
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Level" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      Aeson.toJSON Level.Debug `Hspec.shouldBe` [Aeson.aesonQQ| "debug" |]
      Aeson.toJSON Level.Info `Hspec.shouldBe` [Aeson.aesonQQ| "info" |]
      Aeson.toJSON Level.Warning `Hspec.shouldBe` [Aeson.aesonQQ| "warning" |]
      Aeson.toJSON Level.Error `Hspec.shouldBe` [Aeson.aesonQQ| "error" |]
      Aeson.toJSON Level.Fatal `Hspec.shouldBe` [Aeson.aesonQQ| "fatal" |]
