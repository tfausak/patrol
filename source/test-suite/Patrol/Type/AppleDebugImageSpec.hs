{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.AppleDebugImageSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.AppleDebugImage as AppleDebugImage
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.AppleDebugImage" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let appleDebugImage = AppleDebugImage.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON appleDebugImage `Hspec.shouldBe` json

    Hspec.it "works with an architecture" $ do
      let appleDebugImage = AppleDebugImage.empty {AppleDebugImage.arch = Text.pack "example-arch"}
          json = [Aeson.aesonQQ| { "arch": "example-arch" } |]
      Aeson.toJSON appleDebugImage `Hspec.shouldBe` json

    Hspec.it "works with a CPU subtype" $ do
      let appleDebugImage = AppleDebugImage.empty {AppleDebugImage.cpuSubtype = Just 0}
          json = [Aeson.aesonQQ| { "cpu_subtype": 0 } |]
      Aeson.toJSON appleDebugImage `Hspec.shouldBe` json

    Hspec.it "works with a CPU type" $ do
      let appleDebugImage = AppleDebugImage.empty {AppleDebugImage.cpuType = Just 0}
          json = [Aeson.aesonQQ| { "cpu_type": 0 } |]
      Aeson.toJSON appleDebugImage `Hspec.shouldBe` json

    Hspec.it "works with an image address" $ do
      let appleDebugImage = AppleDebugImage.empty {AppleDebugImage.imageAddr = Text.pack "example-image-addr"}
          json = [Aeson.aesonQQ| { "image_addr": "example-image-addr" } |]
      Aeson.toJSON appleDebugImage `Hspec.shouldBe` json

    Hspec.it "works with an image size" $ do
      let appleDebugImage = AppleDebugImage.empty {AppleDebugImage.imageSize = Just 0}
          json = [Aeson.aesonQQ| { "image_size": 0 } |]
      Aeson.toJSON appleDebugImage `Hspec.shouldBe` json

    Hspec.it "works with an image VM address" $ do
      let appleDebugImage = AppleDebugImage.empty {AppleDebugImage.imageVmaddr = Text.pack "example-image-vmaddr"}
          json = [Aeson.aesonQQ| { "image_vmaddr": "example-image-vmaddr" } |]
      Aeson.toJSON appleDebugImage `Hspec.shouldBe` json

    Hspec.it "works with a name" $ do
      let appleDebugImage = AppleDebugImage.empty {AppleDebugImage.name = Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON appleDebugImage `Hspec.shouldBe` json

    Hspec.it "works with a UUID" $ do
      let appleDebugImage = AppleDebugImage.empty {AppleDebugImage.uuid = Text.pack "example-uuid"}
          json = [Aeson.aesonQQ| { "uuid": "example-uuid" } |]
      Aeson.toJSON appleDebugImage `Hspec.shouldBe` json
