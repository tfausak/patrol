{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.BrowserContextSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.BrowserContext as BrowserContext
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.BrowserContext" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let browserContext = BrowserContext.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON browserContext `Hspec.shouldBe` json

    Hspec.it "works with a name" $ do
      let browserContext = BrowserContext.empty {BrowserContext.name = Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON browserContext `Hspec.shouldBe` json

    Hspec.it "works with a version" $ do
      let browserContext = BrowserContext.empty {BrowserContext.version = Text.pack "example-version"}
          json = [Aeson.aesonQQ| { "version": "example-version" } |]
      Aeson.toJSON browserContext `Hspec.shouldBe` json
