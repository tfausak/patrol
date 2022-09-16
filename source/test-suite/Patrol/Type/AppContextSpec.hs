{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.AppContextSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Patrol.Type.AppContext as AppContext
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.AppContext" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let appContext = AppContext.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON appContext `Hspec.shouldBe` json

    Hspec.it "works with an app build" $ do
      let appContext = AppContext.empty {AppContext.appBuild = Text.pack "example-app-build"}
          json = [Aeson.aesonQQ| { "app_build": "example-app-build" } |]
      Aeson.toJSON appContext `Hspec.shouldBe` json

    Hspec.it "works with an app identifier" $ do
      let appContext = AppContext.empty {AppContext.appIdentifier = Text.pack "example-app-identifier"}
          json = [Aeson.aesonQQ| { "app_identifier": "example-app-identifier" } |]
      Aeson.toJSON appContext `Hspec.shouldBe` json

    Hspec.it "works with an app memory" $ do
      let appContext = AppContext.empty {AppContext.appMemory = Just 0}
          json = [Aeson.aesonQQ| { "app_memory": 0 } |]
      Aeson.toJSON appContext `Hspec.shouldBe` json

    Hspec.it "works with an app name" $ do
      let appContext = AppContext.empty {AppContext.appName = Text.pack "example-app-name"}
          json = [Aeson.aesonQQ| { "app_name": "example-app-name" } |]
      Aeson.toJSON appContext `Hspec.shouldBe` json

    Hspec.it "works with an app start time" $ do
      let appContext = AppContext.empty {AppContext.appStartTime = Just $ Time.UTCTime (Time.fromGregorian 1970 1 1) 0}
          json = [Aeson.aesonQQ| { "app_start_time": "1970-01-01T00:00:00Z" } |]
      Aeson.toJSON appContext `Hspec.shouldBe` json

    Hspec.it "works with an app version" $ do
      let appContext = AppContext.empty {AppContext.appVersion = Text.pack "example-app-version"}
          json = [Aeson.aesonQQ| { "app_version": "example-app-version" } |]
      Aeson.toJSON appContext `Hspec.shouldBe` json

    Hspec.it "works with a build type" $ do
      let appContext = AppContext.empty {AppContext.buildType = Text.pack "example-build-type"}
          json = [Aeson.aesonQQ| { "build_type": "example-build-type" } |]
      Aeson.toJSON appContext `Hspec.shouldBe` json

    Hspec.it "works with a device app hash" $ do
      let appContext = AppContext.empty {AppContext.deviceAppHash = Text.pack "example-device-app-hash"}
          json = [Aeson.aesonQQ| { "device_app_hash": "example-device-app-hash" } |]
      Aeson.toJSON appContext `Hspec.shouldBe` json
