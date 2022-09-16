{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.DebugImageSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.AppleDebugImage as AppleDebugImage
import qualified Patrol.Type.DebugImage as DebugImage
import qualified Patrol.Type.NativeDebugImage as NativeDebugImage
import qualified Patrol.Type.ProguardDebugImage as ProguardDebugImage
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.DebugImage" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works with apple" $ do
      let appleDebugImage = AppleDebugImage.empty {AppleDebugImage.arch = Text.pack "example-arch"}
          debugImage = DebugImage.Apple appleDebugImage
          json = [Aeson.aesonQQ| { "arch": "example-arch" } |]
      Aeson.toJSON debugImage `Hspec.shouldBe` json

    Hspec.it "works with native" $ do
      let nativeDebugImage = NativeDebugImage.empty {NativeDebugImage.arch = Text.pack "example-arch"}
          debugImage = DebugImage.Native nativeDebugImage
          json = [Aeson.aesonQQ| { "arch": "example-arch" } |]
      Aeson.toJSON debugImage `Hspec.shouldBe` json

    Hspec.it "works with proguard" $ do
      let proguardDebugImage = ProguardDebugImage.empty {ProguardDebugImage.uuid = Text.pack "example-uuid"}
          debugImage = DebugImage.Proguard proguardDebugImage
          json = [Aeson.aesonQQ| { "uuid": "example-uuid" } |]
      Aeson.toJSON debugImage `Hspec.shouldBe` json

    Hspec.it "works with other" $ do
      let debugImage = DebugImage.Other . Map.singleton (Text.pack "example-key") $ Aeson.Bool True
          json = [Aeson.aesonQQ| { "example-key": true } |]
      Aeson.toJSON debugImage `Hspec.shouldBe` json
