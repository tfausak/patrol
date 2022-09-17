{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.DeviceContextSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Patrol.Type.DeviceContext as DeviceContext
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.DeviceContext" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let deviceContext = DeviceContext.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with arch" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.arch = Text.pack "example-arch"}
          json = [Aeson.aesonQQ| { "arch": "example-arch" } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with batteryLevel" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.batteryLevel = Just 0.1}
          json = [Aeson.aesonQQ| { "battery_level": 0.1 } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with batteryStatus" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.batteryStatus = Text.pack "example-batteryStatus"}
          json = [Aeson.aesonQQ| { "battery_status": "example-batteryStatus" } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with bootTime" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.bootTime = Just $ Time.UTCTime (Time.fromGregorian 1970 1 1) 0}
          json = [Aeson.aesonQQ| { "boot_time": "1970-01-01T00:00:00Z" } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with brand" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.brand = Text.pack "example-brand"}
          json = [Aeson.aesonQQ| { "brand": "example-brand" } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with charging" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.charging = Just True}
          json = [Aeson.aesonQQ| { "charging": true } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with cpuDescription" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.cpuDescription = Text.pack "example-cpuDescription"}
          json = [Aeson.aesonQQ| { "cpu_description": "example-cpuDescription" } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with deviceType" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.deviceType = Text.pack "example-deviceType"}
          json = [Aeson.aesonQQ| { "device_type": "example-deviceType" } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with deviceUniqueIdentifier" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.deviceUniqueIdentifier = Text.pack "example-deviceUniqueIdentifier"}
          json = [Aeson.aesonQQ| { "device_unique_identifier": "example-deviceUniqueIdentifier" } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with externalFreeStorage" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.externalFreeStorage = Just 0}
          json = [Aeson.aesonQQ| { "external_free_storage": 0 } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with externalStorageSize" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.externalStorageSize = Just 0}
          json = [Aeson.aesonQQ| { "external_storage_size": 0 } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with family" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.family = Text.pack "example-family"}
          json = [Aeson.aesonQQ| { "family": "example-family" } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with freeMemory" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.freeMemory = Just 0}
          json = [Aeson.aesonQQ| { "free_memory": 0 } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with freeStorage" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.freeStorage = Just 0}
          json = [Aeson.aesonQQ| { "free_storage": 0 } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with lowMemory" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.lowMemory = Just True}
          json = [Aeson.aesonQQ| { "low_memory": true } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with manufacturer" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.manufacturer = Text.pack "example-manufacturer"}
          json = [Aeson.aesonQQ| { "manufacturer": "example-manufacturer" } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with memorySize" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.memorySize = Just 0}
          json = [Aeson.aesonQQ| { "memory_size": 0 } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with model" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.model = Text.pack "example-model"}
          json = [Aeson.aesonQQ| { "model": "example-model" } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with modelId" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.modelId = Text.pack "example-modelId"}
          json = [Aeson.aesonQQ| { "model_id": "example-modelId" } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with name" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.name = Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with online" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.online = Just True}
          json = [Aeson.aesonQQ| { "online": true } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with orientation" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.orientation = Text.pack "example-orientation"}
          json = [Aeson.aesonQQ| { "orientation": "example-orientation" } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with processorCount" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.processorCount = Just 0}
          json = [Aeson.aesonQQ| { "processor_count": 0 } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with processorFrequency" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.processorFrequency = Just 0.1}
          json = [Aeson.aesonQQ| { "processor_frequency": 0.1 } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with screenDensity" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.screenDensity = Just 0.1}
          json = [Aeson.aesonQQ| { "screen_density": 0.1 } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with screenDpi" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.screenDpi = Just 0.1}
          json = [Aeson.aesonQQ| { "screen_dpi": 0.1 } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with screenResolution" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.screenResolution = Text.pack "example-screenResolution"}
          json = [Aeson.aesonQQ| { "screen_resolution": "example-screenResolution" } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with simulator" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.simulator = Just True}
          json = [Aeson.aesonQQ| { "simulator": true } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with storageSize" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.storageSize = Just 0}
          json = [Aeson.aesonQQ| { "storage_size": 0 } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with supportsAccelerometer" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.supportsAccelerometer = Just True}
          json = [Aeson.aesonQQ| { "supports_accelerometer": true } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with supportsAudio" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.supportsAudio = Just True}
          json = [Aeson.aesonQQ| { "supports_audio": true } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with supportsGyroscope" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.supportsGyroscope = Just True}
          json = [Aeson.aesonQQ| { "supports_gyroscope": true } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with supportsLocationService" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.supportsLocationService = Just True}
          json = [Aeson.aesonQQ| { "supports_location_service": true } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with supportsVibration" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.supportsVibration = Just True}
          json = [Aeson.aesonQQ| { "supports_vibration": true } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with timezone" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.timezone = Text.pack "example-timezone"}
          json = [Aeson.aesonQQ| { "timezone": "example-timezone" } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json

    Hspec.it "works with usableMemory" $ do
      let deviceContext = DeviceContext.empty {DeviceContext.usableMemory = Just 0}
          json = [Aeson.aesonQQ| { "usable_memory": 0 } |]
      Aeson.toJSON deviceContext `Hspec.shouldBe` json
