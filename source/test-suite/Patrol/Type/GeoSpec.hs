{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.GeoSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.Geo as Geo
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Geo" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let geo = Geo.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON geo `Hspec.shouldBe` json

    Hspec.it "works with a city" $ do
      let geo = Geo.empty {Geo.city = Text.pack "example-city"}
          json = [Aeson.aesonQQ| { "city": "example-city" } |]
      Aeson.toJSON geo `Hspec.shouldBe` json

    Hspec.it "works with a country code" $ do
      let geo = Geo.empty {Geo.countryCode = Text.pack "example-country-code"}
          json = [Aeson.aesonQQ| { "country_code": "example-country-code" } |]
      Aeson.toJSON geo `Hspec.shouldBe` json

    Hspec.it "works with a region" $ do
      let geo = Geo.empty {Geo.region = Text.pack "example-region"}
          json = [Aeson.aesonQQ| { "region": "example-region" } |]
      Aeson.toJSON geo `Hspec.shouldBe` json
