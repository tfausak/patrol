{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ProguardDebugImageSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.ProguardDebugImage as ProguardDebugImage
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.ProguardDebugImage" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let proguardDebugImage = ProguardDebugImage.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON proguardDebugImage `Hspec.shouldBe` json

    Hspec.it "works with a UUID" $ do
      let proguardDebugImage = ProguardDebugImage.empty {ProguardDebugImage.uuid = Text.pack "example-uuid"}
          json = [Aeson.aesonQQ| { "uuid": "example-uuid" } |]
      Aeson.toJSON proguardDebugImage `Hspec.shouldBe` json
