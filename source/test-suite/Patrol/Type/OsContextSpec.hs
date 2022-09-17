{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.OsContextSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.OsContext as OsContext
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.OsContext" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let osContext = OsContext.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON osContext `Hspec.shouldBe` json

    Hspec.it "works with build" $ do
      let osContext = OsContext.empty {OsContext.build = Text.pack "example-build"}
          json = [Aeson.aesonQQ| { "build": "example-build" } |]
      Aeson.toJSON osContext `Hspec.shouldBe` json

    Hspec.it "works with kernelVersion" $ do
      let osContext = OsContext.empty {OsContext.kernelVersion = Text.pack "example-kernelVersion"}
          json = [Aeson.aesonQQ| { "kernel_version": "example-kernelVersion" } |]
      Aeson.toJSON osContext `Hspec.shouldBe` json

    Hspec.it "works with name" $ do
      let osContext = OsContext.empty {OsContext.name = Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON osContext `Hspec.shouldBe` json

    Hspec.it "works with rawDescription" $ do
      let osContext = OsContext.empty {OsContext.rawDescription = Text.pack "example-rawDescription"}
          json = [Aeson.aesonQQ| { "raw_description": "example-rawDescription" } |]
      Aeson.toJSON osContext `Hspec.shouldBe` json

    Hspec.it "works with rooted" $ do
      let osContext = OsContext.empty {OsContext.rooted = Just True}
          json = [Aeson.aesonQQ| { "rooted": true } |]
      Aeson.toJSON osContext `Hspec.shouldBe` json

    Hspec.it "works with version" $ do
      let osContext = OsContext.empty {OsContext.version = Text.pack "example-version"}
          json = [Aeson.aesonQQ| { "version": "example-version" } |]
      Aeson.toJSON osContext `Hspec.shouldBe` json
