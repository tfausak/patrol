module Patrol.Type.LevelSpec where

import qualified Data.Aeson as Aeson
import qualified Patrol.Type.Level as Level
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Level" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      Aeson.encode Level.Debug `Hspec.shouldBe` Aeson.encode "debug"
      Aeson.encode Level.Info `Hspec.shouldBe` Aeson.encode "info"
      Aeson.encode Level.Warning `Hspec.shouldBe` Aeson.encode "warning"
      Aeson.encode Level.Error `Hspec.shouldBe` Aeson.encode "error"
      Aeson.encode Level.Fatal `Hspec.shouldBe` Aeson.encode "fatal"
