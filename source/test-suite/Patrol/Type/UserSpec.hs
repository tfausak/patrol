{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.UserSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.Geo as Geo
import qualified Patrol.Type.User as User
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.User" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let user = User.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON user `Hspec.shouldBe` json

    Hspec.it "works with some data" $ do
      let user = User.empty {User.data_ = Map.singleton (Text.pack "example-data") (Aeson.Bool True)}
          json = [Aeson.aesonQQ| { "data": { "example-data": true } } |]
      Aeson.toJSON user `Hspec.shouldBe` json

    Hspec.it "works with an email" $ do
      let user = User.empty {User.email = Text.pack "example-email"}
          json = [Aeson.aesonQQ| { "email": "example-email" } |]
      Aeson.toJSON user `Hspec.shouldBe` json

    Hspec.it "works with a geo" $ do
      let geo = Geo.empty {Geo.city = Text.pack "example-city"}
          user = User.empty {User.geo = Just geo}
          json = [Aeson.aesonQQ| { "geo": { "city": "example-city" } } |]
      Aeson.toJSON user `Hspec.shouldBe` json

    Hspec.it "works with an id" $ do
      let user = User.empty {User.id = Text.pack "example-id"}
          json = [Aeson.aesonQQ| { "id": "example-id" } |]
      Aeson.toJSON user `Hspec.shouldBe` json

    Hspec.it "works with an IP address" $ do
      let user = User.empty {User.ipAddress = Text.pack "example-ip-address"}
          json = [Aeson.aesonQQ| { "ip_address": "example-ip-address" } |]
      Aeson.toJSON user `Hspec.shouldBe` json

    Hspec.it "works with a name" $ do
      let user = User.empty {User.name = Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON user `Hspec.shouldBe` json

    Hspec.it "works with a segment" $ do
      let user = User.empty {User.segment = Text.pack "example-segment"}
          json = [Aeson.aesonQQ| { "segment": "example-segment" } |]
      Aeson.toJSON user `Hspec.shouldBe` json

    Hspec.it "works with a username" $ do
      let user = User.empty {User.username = Text.pack "example-username"}
          json = [Aeson.aesonQQ| { "username": "example-username" } |]
      Aeson.toJSON user `Hspec.shouldBe` json
