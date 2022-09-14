{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.RequestSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.Request as Request
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Request" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let request = Request.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON request `Hspec.shouldBe` json

    Hspec.it "works with a cookie" $ do
      let request = Request.empty {Request.cookies = Map.singleton (Text.pack "example-key") . Just $ Text.pack "example-value"}
          json = [Aeson.aesonQQ| { "cookies": { "example-key": "example-value" } } |]
      Aeson.toJSON request `Hspec.shouldBe` json

    Hspec.it "works with some data" $ do
      let request = Request.empty {Request.data_ = Aeson.Bool True}
          json = [Aeson.aesonQQ| { "data": true } |]
      Aeson.toJSON request `Hspec.shouldBe` json

    Hspec.it "works with an env" $ do
      let request = Request.empty {Request.env = Map.singleton (Text.pack "example-key") $ Aeson.Bool True}
          json = [Aeson.aesonQQ| { "env": { "example-key": true } } |]
      Aeson.toJSON request `Hspec.shouldBe` json

    Hspec.it "works with a fragment" $ do
      let request = Request.empty {Request.fragment = Just $ Text.pack "example-fragment"}
          json = [Aeson.aesonQQ| { "fragment": "example-fragment" } |]
      Aeson.toJSON request `Hspec.shouldBe` json

    Hspec.it "works with a header" $ do
      let request = Request.empty {Request.headers = Map.singleton (Text.pack "example-key") . Just $ Text.pack "example-value"}
          json = [Aeson.aesonQQ| { "headers": { "example-key": "example-value" } } |]
      Aeson.toJSON request `Hspec.shouldBe` json

    Hspec.it "works with an inferred content type" $ do
      let request = Request.empty {Request.inferredContentType = Just $ Text.pack "example-inferred-content-type"}
          json = [Aeson.aesonQQ| { "inferred_content_type": "example-inferred-content-type" } |]
      Aeson.toJSON request `Hspec.shouldBe` json

    Hspec.it "works with a method" $ do
      let request = Request.empty {Request.method = Just $ Text.pack "example-method"}
          json = [Aeson.aesonQQ| { "method": "example-method" } |]
      Aeson.toJSON request `Hspec.shouldBe` json

    Hspec.it "works with a query string" $ do
      let request = Request.empty {Request.queryString = Map.singleton (Text.pack "example-key") . Just $ Text.pack "example-value"}
          json = [Aeson.aesonQQ| { "query_string": { "example-key": "example-value" } } |]
      Aeson.toJSON request `Hspec.shouldBe` json

    Hspec.it "works with a URL" $ do
      let request = Request.empty {Request.url = Just $ Text.pack "example-url"}
          json = [Aeson.aesonQQ| { "url": "example-url" } |]
      Aeson.toJSON request `Hspec.shouldBe` json
