{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ClientSdkPackageSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.ClientSdkPackage as ClientSdkPackage
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.ClientSdkPackage" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let clientSdkPackage = ClientSdkPackage.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON clientSdkPackage `Hspec.shouldBe` json

    Hspec.it "works with a name" $ do
      let clientSdkPackage = ClientSdkPackage.empty {ClientSdkPackage.name = Just $ Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON clientSdkPackage `Hspec.shouldBe` json

    Hspec.it "works with a version" $ do
      let clientSdkPackage = ClientSdkPackage.empty {ClientSdkPackage.version = Just $ Text.pack "example-version"}
          json = [Aeson.aesonQQ| { "version": "example-version" } |]
      Aeson.toJSON clientSdkPackage `Hspec.shouldBe` json
