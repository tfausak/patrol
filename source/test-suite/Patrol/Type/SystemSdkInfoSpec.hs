{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.SystemSdkInfoSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.SystemSdkInfo as SystemSdkInfo
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.SystemSdkInfo" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let systemSdkInfo = SystemSdkInfo.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON systemSdkInfo `Hspec.shouldBe` json

    Hspec.it "works with an SDK name" $ do
      let systemSdkInfo = SystemSdkInfo.empty {SystemSdkInfo.sdkName = Text.pack "example-sdk-name"}
          json = [Aeson.aesonQQ| { "sdk_name": "example-sdk-name" } |]
      Aeson.toJSON systemSdkInfo `Hspec.shouldBe` json

    Hspec.it "works with a major version number" $ do
      let systemSdkInfo = SystemSdkInfo.empty {SystemSdkInfo.versionMajor = Just 0}
          json = [Aeson.aesonQQ| { "version_major": 0 } |]
      Aeson.toJSON systemSdkInfo `Hspec.shouldBe` json

    Hspec.it "works with a minor version number" $ do
      let systemSdkInfo = SystemSdkInfo.empty {SystemSdkInfo.versionMinor = Just 0}
          json = [Aeson.aesonQQ| { "version_minor": 0 } |]
      Aeson.toJSON systemSdkInfo `Hspec.shouldBe` json

    Hspec.it "works with a patch version number" $ do
      let systemSdkInfo = SystemSdkInfo.empty {SystemSdkInfo.versionPatchlevel = Just 0}
          json = [Aeson.aesonQQ| { "version_patchlevel": 0 } |]
      Aeson.toJSON systemSdkInfo `Hspec.shouldBe` json
