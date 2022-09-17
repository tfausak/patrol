{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.RuntimeContextSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.RuntimeContext as RuntimeContext
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.RuntimeContext" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let runtimeContext = RuntimeContext.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON runtimeContext `Hspec.shouldBe` json

    Hspec.it "works with build" $ do
      let runtimeContext = RuntimeContext.empty {RuntimeContext.build = Text.pack "example-build"}
          json = [Aeson.aesonQQ| { "build": "example-build" } |]
      Aeson.toJSON runtimeContext `Hspec.shouldBe` json

    Hspec.it "works with name" $ do
      let runtimeContext = RuntimeContext.empty {RuntimeContext.name = Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON runtimeContext `Hspec.shouldBe` json

    Hspec.it "works with rawDescription" $ do
      let runtimeContext = RuntimeContext.empty {RuntimeContext.rawDescription = Text.pack "example-rawDescription"}
          json = [Aeson.aesonQQ| { "raw_description": "example-rawDescription" } |]
      Aeson.toJSON runtimeContext `Hspec.shouldBe` json

    Hspec.it "works with version" $ do
      let runtimeContext = RuntimeContext.empty {RuntimeContext.version = Text.pack "example-version"}
          json = [Aeson.aesonQQ| { "version": "example-version" } |]
      Aeson.toJSON runtimeContext `Hspec.shouldBe` json
