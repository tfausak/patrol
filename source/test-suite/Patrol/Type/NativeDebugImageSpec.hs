{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.NativeDebugImageSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.NativeDebugImage as NativeDebugImage
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.NativeDebugImage" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let nativeDebugImage = NativeDebugImage.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON nativeDebugImage `Hspec.shouldBe` json

    Hspec.it "works with an arch" $ do
      let nativeDebugImage = NativeDebugImage.empty {NativeDebugImage.arch = Text.pack "example-arch"}
          json = [Aeson.aesonQQ| { "arch": "example-arch" } |]
      Aeson.toJSON nativeDebugImage `Hspec.shouldBe` json

    Hspec.it "works with a code file" $ do
      let nativeDebugImage = NativeDebugImage.empty {NativeDebugImage.codeFile = Text.pack "example-code-file"}
          json = [Aeson.aesonQQ| { "code_file": "example-code-file" } |]
      Aeson.toJSON nativeDebugImage `Hspec.shouldBe` json

    Hspec.it "works with a code id" $ do
      let nativeDebugImage = NativeDebugImage.empty {NativeDebugImage.codeId = Text.pack "example-code-id"}
          json = [Aeson.aesonQQ| { "code_id": "example-code-id" } |]
      Aeson.toJSON nativeDebugImage `Hspec.shouldBe` json

    Hspec.it "works with a debug file" $ do
      let nativeDebugImage = NativeDebugImage.empty {NativeDebugImage.debugFile = Text.pack "example-debug-file"}
          json = [Aeson.aesonQQ| { "debug_file": "example-debug-file" } |]
      Aeson.toJSON nativeDebugImage `Hspec.shouldBe` json

    Hspec.it "works with a debug id" $ do
      let nativeDebugImage = NativeDebugImage.empty {NativeDebugImage.debugId = Text.pack "example-debug-id"}
          json = [Aeson.aesonQQ| { "debug_id": "example-debug-id" } |]
      Aeson.toJSON nativeDebugImage `Hspec.shouldBe` json

    Hspec.it "works with a image addr" $ do
      let nativeDebugImage = NativeDebugImage.empty {NativeDebugImage.imageAddr = Text.pack "example-image-addr"}
          json = [Aeson.aesonQQ| { "image_addr": "example-image-addr" } |]
      Aeson.toJSON nativeDebugImage `Hspec.shouldBe` json

    Hspec.it "works with a image size" $ do
      let nativeDebugImage = NativeDebugImage.empty {NativeDebugImage.imageSize = Just 0}
          json = [Aeson.aesonQQ| { "image_size": 0 } |]
      Aeson.toJSON nativeDebugImage `Hspec.shouldBe` json

    Hspec.it "works with a image vmaddr" $ do
      let nativeDebugImage = NativeDebugImage.empty {NativeDebugImage.imageVmaddr = Text.pack "example-image-vmaddr"}
          json = [Aeson.aesonQQ| { "image_vmaddr": "example-image-vmaddr" } |]
      Aeson.toJSON nativeDebugImage `Hspec.shouldBe` json

    Hspec.it "works with a type" $ do
      let nativeDebugImage = NativeDebugImage.empty {NativeDebugImage.type_ = Text.pack "example-type"}
          json = [Aeson.aesonQQ| { "type": "example-type" } |]
      Aeson.toJSON nativeDebugImage `Hspec.shouldBe` json
