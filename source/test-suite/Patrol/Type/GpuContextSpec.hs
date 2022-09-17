{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.GpuContextSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.GpuContext as GpuContext
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.GpuContext" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let gpuContext = GpuContext.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON gpuContext `Hspec.shouldBe` json

    Hspec.it "works with apiType" $ do
      let gpuContext = GpuContext.empty {GpuContext.apiType = Text.pack "example-apiType"}
          json = [Aeson.aesonQQ| { "api_type": "example-apiType" } |]
      Aeson.toJSON gpuContext `Hspec.shouldBe` json

    Hspec.it "works with graphicsShaderLevel" $ do
      let gpuContext = GpuContext.empty {GpuContext.graphicsShaderLevel = Text.pack "example-graphicsShaderLevel"}
          json = [Aeson.aesonQQ| { "graphics_shader_level": "example-graphicsShaderLevel" } |]
      Aeson.toJSON gpuContext `Hspec.shouldBe` json

    Hspec.it "works with id" $ do
      let gpuContext = GpuContext.empty {GpuContext.id = Aeson.Bool True}
          json = [Aeson.aesonQQ| { "id": true } |]
      Aeson.toJSON gpuContext `Hspec.shouldBe` json

    Hspec.it "works with maxTextureSize" $ do
      let gpuContext = GpuContext.empty {GpuContext.maxTextureSize = Just 0}
          json = [Aeson.aesonQQ| { "max_texture_size": 0 } |]
      Aeson.toJSON gpuContext `Hspec.shouldBe` json

    Hspec.it "works with memorySize" $ do
      let gpuContext = GpuContext.empty {GpuContext.memorySize = Just 0.1}
          json = [Aeson.aesonQQ| { "memory_size": 0.1 } |]
      Aeson.toJSON gpuContext `Hspec.shouldBe` json

    Hspec.it "works with multiThreadedRendering" $ do
      let gpuContext = GpuContext.empty {GpuContext.multiThreadedRendering = Just True}
          json = [Aeson.aesonQQ| { "multi_threaded_rendering": true } |]
      Aeson.toJSON gpuContext `Hspec.shouldBe` json

    Hspec.it "works with name" $ do
      let gpuContext = GpuContext.empty {GpuContext.name = Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON gpuContext `Hspec.shouldBe` json

    Hspec.it "works with npotSupport" $ do
      let gpuContext = GpuContext.empty {GpuContext.npotSupport = Text.pack "example-npotSupport"}
          json = [Aeson.aesonQQ| { "npot_support": "example-npotSupport" } |]
      Aeson.toJSON gpuContext `Hspec.shouldBe` json

    Hspec.it "works with supportsComputeShaders" $ do
      let gpuContext = GpuContext.empty {GpuContext.supportsComputeShaders = Just True}
          json = [Aeson.aesonQQ| { "supports_compute_shaders": true } |]
      Aeson.toJSON gpuContext `Hspec.shouldBe` json

    Hspec.it "works with supportsGeometryShaders" $ do
      let gpuContext = GpuContext.empty {GpuContext.supportsGeometryShaders = Just True}
          json = [Aeson.aesonQQ| { "supports_geometry_shaders": true } |]
      Aeson.toJSON gpuContext `Hspec.shouldBe` json

    Hspec.it "works with supportsRayTracing" $ do
      let gpuContext = GpuContext.empty {GpuContext.supportsRayTracing = Just True}
          json = [Aeson.aesonQQ| { "supports_ray_tracing": true } |]
      Aeson.toJSON gpuContext `Hspec.shouldBe` json

    Hspec.it "works with vendorId" $ do
      let gpuContext = GpuContext.empty {GpuContext.vendorId = Text.pack "example-vendorId"}
          json = [Aeson.aesonQQ| { "vendor_id": "example-vendorId" } |]
      Aeson.toJSON gpuContext `Hspec.shouldBe` json

    Hspec.it "works with vendorName" $ do
      let gpuContext = GpuContext.empty {GpuContext.vendorName = Text.pack "example-vendorName"}
          json = [Aeson.aesonQQ| { "vendor_name": "example-vendorName" } |]
      Aeson.toJSON gpuContext `Hspec.shouldBe` json

    Hspec.it "works with version" $ do
      let gpuContext = GpuContext.empty {GpuContext.version = Text.pack "example-version"}
          json = [Aeson.aesonQQ| { "version": "example-version" } |]
      Aeson.toJSON gpuContext `Hspec.shouldBe` json
