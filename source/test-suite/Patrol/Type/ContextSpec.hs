{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ContextSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.AppContext as AppContext
import qualified Patrol.Type.BrowserContext as BrowserContext
import qualified Patrol.Type.Context as Context
import qualified Patrol.Type.DeviceContext as DeviceContext
import qualified Patrol.Type.GpuContext as GpuContext
import qualified Patrol.Type.OsContext as OsContext
import qualified Patrol.Type.RuntimeContext as RuntimeContext
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Context" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works with app" $ do
      let context = Context.App AppContext.empty {AppContext.appBuild = Text.pack "example-app-build"}
          json = [Aeson.aesonQQ| { "app_build": "example-app-build" } |]
      Aeson.toJSON context `Hspec.shouldBe` json

    Hspec.it "works with browser" $ do
      let context = Context.Browser BrowserContext.empty {BrowserContext.name = Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON context `Hspec.shouldBe` json

    Hspec.it "works with device" $ do
      let context = Context.Device DeviceContext.empty {DeviceContext.name = Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON context `Hspec.shouldBe` json

    Hspec.it "works with gpu" $ do
      let context = Context.Gpu GpuContext.empty {GpuContext.name = Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON context `Hspec.shouldBe` json

    Hspec.it "works with os" $ do
      let context = Context.Os OsContext.empty {OsContext.name = Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON context `Hspec.shouldBe` json

    Hspec.it "works with runtime" $ do
      let context = Context.Runtime RuntimeContext.empty {RuntimeContext.name = Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON context `Hspec.shouldBe` json

    Hspec.it "works with other" $ do
      let context = Context.Other . Map.singleton (Text.pack "example-key") $ Aeson.Bool True
          json = [Aeson.aesonQQ| { "example-key": true } |]
      Aeson.toJSON context `Hspec.shouldBe` json
