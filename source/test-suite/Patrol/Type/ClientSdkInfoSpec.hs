{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ClientSdkInfoSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Constant as Constant
import qualified Patrol.Type.ClientSdkInfo as ClientSdkInfo
import qualified Patrol.Type.ClientSdkPackage as ClientSdkPackage
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.ClientSdkInfo" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let clientSdkInfo = ClientSdkInfo.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON clientSdkInfo `Hspec.shouldBe` json

    Hspec.it "works with an integration" $ do
      let clientSdkInfo = ClientSdkInfo.empty {ClientSdkInfo.integrations = [Text.pack "example-integration"]}
          json = [Aeson.aesonQQ| { "integrations": [ "example-integration" ] } |]
      Aeson.toJSON clientSdkInfo `Hspec.shouldBe` json

    Hspec.it "works with a name" $ do
      let clientSdkInfo = ClientSdkInfo.empty {ClientSdkInfo.name = Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON clientSdkInfo `Hspec.shouldBe` json

    Hspec.it "works with a package" $ do
      let clientSdkPackage = ClientSdkPackage.empty {ClientSdkPackage.name = Text.pack "example-name"}
          clientSdkInfo = ClientSdkInfo.empty {ClientSdkInfo.packages = [clientSdkPackage]}
          json = [Aeson.aesonQQ| { "packages": [ { "name": "example-name" } ] } |]
      Aeson.toJSON clientSdkInfo `Hspec.shouldBe` json

    Hspec.it "works with a version" $ do
      let clientSdkInfo = ClientSdkInfo.empty {ClientSdkInfo.version = Text.pack "example-version"}
          json = [Aeson.aesonQQ| { "version": "example-version" } |]
      Aeson.toJSON clientSdkInfo `Hspec.shouldBe` json

  Hspec.describe "patrol" $ do
    Hspec.it "agrees with the user agent" $ do
      let actual = ClientSdkInfo.name ClientSdkInfo.patrol <> "/" <> ClientSdkInfo.version ClientSdkInfo.patrol
      actual `Hspec.shouldBe` Constant.userAgent
