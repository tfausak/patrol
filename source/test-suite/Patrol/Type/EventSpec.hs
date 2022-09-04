{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.EventSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as Uuid
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.URI.Static as Uri
import qualified Patrol.Constant as Constant
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.Event as Event
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Level as Level
import qualified Patrol.Type.Platform as Platform
import qualified Patrol.Type.Timestamp as Timestamp
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Event" $ do
  Hspec.describe "new" $ do
    Hspec.it "generates an ID" $ do
      event <- Event.new
      Event.id event `Hspec.shouldNotBe` EventId.fromUuid Uuid.nil

    Hspec.it "sets the timestamp" $ do
      event <- Event.new
      Event.timestamp event `Hspec.shouldSatisfy` Maybe.isJust

    Hspec.it "sets the platform" $ do
      event <- Event.new
      Event.platform event `Hspec.shouldBe` Just Platform.Haskell

    Hspec.it "sets the level" $ do
      event <- Event.new
      Event.level event `Hspec.shouldBe` Just Level.Error

  Hspec.describe "ToJSON" $ do
    let emptyEvent =
          Event.Event
            { Event.dist = Nothing,
              Event.id = EventId.fromUuid Uuid.nil,
              Event.level = Nothing,
              Event.logger = Nothing,
              Event.platform = Nothing,
              Event.release = Nothing,
              Event.serverName = Nothing,
              Event.timestamp = Nothing,
              Event.transaction = Nothing
            }

    Hspec.it "works" $ do
      let lazyByteString = LazyByteString.fromStrict . Text.encodeUtf8 $ Text.pack "{\"event_id\":\"00000000000000000000000000000000\"}"
      Aeson.encode emptyEvent `Hspec.shouldBe` lazyByteString

    Hspec.it "works with dist" $ do
      let event = emptyEvent {Event.dist = Just $ Text.pack "example-dist"}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "dist": "example-dist" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with level" $ do
      let event = emptyEvent {Event.level = Just Level.Error}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "level": "error" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with logger" $ do
      let event = emptyEvent {Event.logger = Just $ Text.pack "example-logger"}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "logger": "example-logger" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with platform" $ do
      let event = emptyEvent {Event.platform = Just Platform.Haskell}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "platform": "haskell" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with release" $ do
      let event = emptyEvent {Event.release = Just $ Text.pack "example-release"}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "release": "example-release" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with server name" $ do
      let event = emptyEvent {Event.serverName = Just $ Text.pack "example-server-name"}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "server_name": "example-server-name" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with timestamp" $ do
      let event = emptyEvent {Event.timestamp = Just Timestamp.epoch}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "timestamp": "1970-01-01T00:00:00Z" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with transaction" $ do
      let event = emptyEvent {Event.transaction = Just $ Text.pack "example-transaction"}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "transaction": "example-transaction" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

  Hspec.describe "intoRequest" $ do
    Hspec.it "sets the method" $ do
      dsn <- maybe (fail "invalid DSN") pure $ Dsn.fromUri [Uri.uri|http://public-key@sentry.test/project-id|]
      event <- Event.new
      request <- Event.intoRequest dsn event
      Client.method request `Hspec.shouldBe` Http.methodPost

    Hspec.it "sets the host" $ do
      dsn <- maybe (fail "invalid DSN") pure $ Dsn.fromUri [Uri.uri|http://public-key@sentry.test:8080/project-id|]
      event <- Event.new
      request <- Event.intoRequest dsn event
      Client.host request `Hspec.shouldBe` Text.encodeUtf8 (Text.pack "sentry.test")

    Hspec.it "sets the port" $ do
      dsn <- maybe (fail "invalid DSN") pure $ Dsn.fromUri [Uri.uri|http://public-key@sentry.test:8080/project-id|]
      event <- Event.new
      request <- Event.intoRequest dsn event
      Client.port request `Hspec.shouldBe` 8080

    Hspec.it "sets the path" $ do
      dsn <- maybe (fail "invalid DSN") pure $ Dsn.fromUri [Uri.uri|http://public-key@sentry.test/project-id|]
      event <- Event.new
      request <- Event.intoRequest dsn event
      Client.path request `Hspec.shouldBe` Text.encodeUtf8 (Text.pack "/api/project-id/store/")

    Hspec.it "handles a custom path" $ do
      dsn <- maybe (fail "invalid DSN") pure $ Dsn.fromUri [Uri.uri|http://public-key@sentry.test/custom/project-id|]
      event <- Event.new
      request <- Event.intoRequest dsn event
      Client.path request `Hspec.shouldBe` Text.encodeUtf8 (Text.pack "/custom/api/project-id/store/")

    Hspec.it "sets the body" $ do
      dsn <- maybe (fail "invalid DSN") pure $ Dsn.fromUri [Uri.uri|http://public-key@sentry.test/project-id|]
      event <- Event.new
      request <- Event.intoRequest dsn event
      case Client.requestBody request of
        Client.RequestBodyBS byteString -> byteString `Hspec.shouldBe` LazyByteString.toStrict (Aeson.encode event)
        _ -> fail "unexpected request body"

    Hspec.it "sets the content type" $ do
      dsn <- maybe (fail "invalid DSN") pure $ Dsn.fromUri [Uri.uri|http://public-key@sentry.test/project-id|]
      event <- Event.new
      request <- Event.intoRequest dsn event
      lookup Http.hContentType (Client.requestHeaders request) `Hspec.shouldSatisfy` Maybe.isJust

    Hspec.it "sets the user agent" $ do
      dsn <- maybe (fail "invalid DSN") pure $ Dsn.fromUri [Uri.uri|http://public-key@sentry.test/project-id|]
      event <- Event.new
      request <- Event.intoRequest dsn event
      lookup Http.hUserAgent (Client.requestHeaders request) `Hspec.shouldSatisfy` Maybe.isJust

    Hspec.it "sets the authorization" $ do
      dsn <- maybe (fail "invalid DSN") pure $ Dsn.fromUri [Uri.uri|http://public-key@sentry.test/project-id|]
      event <- Event.new
      request <- Event.intoRequest dsn event
      lookup Constant.xSentryAuth (Client.requestHeaders request) `Hspec.shouldSatisfy` Maybe.isJust
