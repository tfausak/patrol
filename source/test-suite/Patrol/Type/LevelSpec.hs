{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.LevelSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Patrol.Type.Level as Level
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Level" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works for Debug" $ do
      Aeson.toJSON Level.Debug `Hspec.shouldBe` [Aeson.aesonQQ| "debug" |]

    Hspec.it "works for Info" $ do
      Aeson.toJSON Level.Info `Hspec.shouldBe` [Aeson.aesonQQ| "info" |]

    Hspec.it "works for Warning" $ do
      Aeson.toJSON Level.Warning `Hspec.shouldBe` [Aeson.aesonQQ| "warning" |]

    Hspec.it "works for Error" $ do
      Aeson.toJSON Level.Error `Hspec.shouldBe` [Aeson.aesonQQ| "error" |]

    Hspec.it "works for Fatal" $ do
      Aeson.toJSON Level.Fatal `Hspec.shouldBe` [Aeson.aesonQQ| "fatal" |]
