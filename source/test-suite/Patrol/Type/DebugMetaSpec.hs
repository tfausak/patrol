{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.DebugMetaSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.DebugMeta as DebugMeta
import qualified Patrol.Type.SystemSdkInfo as SystemSdkInfo
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.DebugMeta" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let debugMeta = DebugMeta.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON debugMeta `Hspec.shouldBe` json

    Hspec.it "works with an image" $ do
      let image = Map.singleton (Text.pack "example-image") $ Aeson.Bool True
          debugMeta = DebugMeta.empty {DebugMeta.images = [image]}
          json = [Aeson.aesonQQ| { "images": [ { "example-image": true } ] } |]
      Aeson.toJSON debugMeta `Hspec.shouldBe` json

    Hspec.it "works with some SDK info" $ do
      let systemSdkInfo = SystemSdkInfo.empty {SystemSdkInfo.versionMajor = Just 0}
          debugMeta = DebugMeta.empty {DebugMeta.sdkInfo = Just systemSdkInfo}
          json = [Aeson.aesonQQ| { "sdk_info": { "version_major": 0 } } |]
      Aeson.toJSON debugMeta `Hspec.shouldBe` json
