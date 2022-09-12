{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.BreadcrumbSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Patrol.Type.Breadcrumb as Breadcrumb
import qualified Patrol.Type.BreadcrumbType as BreadcrumbType
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Level as Level
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Breadcrumb" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let breadcrumb = Breadcrumb.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON breadcrumb `Hspec.shouldBe` json

    Hspec.it "works with a category" $ do
      let breadcrumb = Breadcrumb.empty {Breadcrumb.category = Just $ Text.pack "example-category"}
          json = [Aeson.aesonQQ| { "category": "example-category" } |]
      Aeson.toJSON breadcrumb `Hspec.shouldBe` json

    Hspec.it "works with some data" $ do
      let breadcrumb = Breadcrumb.empty {Breadcrumb.data_ = Map.singleton (Text.pack "example-data") (Aeson.Bool True)}
          json = [Aeson.aesonQQ| { "data": { "example-data": true } } |]
      Aeson.toJSON breadcrumb `Hspec.shouldBe` json

    Hspec.it "works with an event ID" $ do
      let breadcrumb = Breadcrumb.empty {Breadcrumb.eventId = Just EventId.empty}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000" } |]
      Aeson.toJSON breadcrumb `Hspec.shouldBe` json

    Hspec.it "works with a level" $ do
      let breadcrumb = Breadcrumb.empty {Breadcrumb.level = Just Level.Debug}
          json = [Aeson.aesonQQ| { "level": "debug" } |]
      Aeson.toJSON breadcrumb `Hspec.shouldBe` json

    Hspec.it "works with a message" $ do
      let breadcrumb = Breadcrumb.empty {Breadcrumb.message = Just $ Text.pack "example-message"}
          json = [Aeson.aesonQQ| { "message": "example-message" } |]
      Aeson.toJSON breadcrumb `Hspec.shouldBe` json

    Hspec.it "works with a timestamp" $ do
      let breadcrumb = Breadcrumb.empty {Breadcrumb.timestamp = Just $ Time.UTCTime (Time.fromGregorian 1970 1 1) 0}
          json = [Aeson.aesonQQ| { "timestamp": "1970-01-01T00:00:00Z" } |]
      Aeson.toJSON breadcrumb `Hspec.shouldBe` json

    Hspec.it "works with a type" $ do
      let breadcrumb = Breadcrumb.empty {Breadcrumb.type_ = Just BreadcrumbType.Default}
          json = [Aeson.aesonQQ| { "type": "default" } |]
      Aeson.toJSON breadcrumb `Hspec.shouldBe` json
