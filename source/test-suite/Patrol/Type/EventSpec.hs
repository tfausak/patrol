{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- In the use of ‘intoRequest’ (imported from Patrol.Type.Event)
{-# OPTIONS_GHC -Wno-deprecations #-}

module Patrol.Type.EventSpec where

import qualified Control.Exception as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Patrol.Constant as Constant
import qualified Patrol.Type.Breadcrumb as Breadcrumb
import qualified Patrol.Type.Breadcrumbs as Breadcrumbs
import qualified Patrol.Type.ClientSdkInfo as ClientSdkInfo
import qualified Patrol.Type.Context as Context
import qualified Patrol.Type.DebugImage as DebugImage
import qualified Patrol.Type.DebugMeta as DebugMeta
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.Event as Event
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.EventProcessingError as EventProcessingError
import qualified Patrol.Type.EventType as EventType
import qualified Patrol.Type.Exception as Exception
import qualified Patrol.Type.Exceptions as Exceptions
import qualified Patrol.Type.Level as Level
import qualified Patrol.Type.LogEntry as LogEntry
import qualified Patrol.Type.Platform as Platform
import qualified Patrol.Type.Request as Request
import qualified Patrol.Type.TransactionInfo as TransactionInfo
import qualified Patrol.Type.User as User
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Event" $ do
  Hspec.describe "new" $ do
    Hspec.it "generates an ID" $ do
      event <- Event.new
      Event.eventId event `Hspec.shouldNotBe` EventId.empty

    Hspec.it "sets the timestamp" $ do
      event <- Event.new
      Event.timestamp event `Hspec.shouldSatisfy` Maybe.isJust

    Hspec.it "sets the platform" $ do
      event <- Event.new
      Event.platform event `Hspec.shouldBe` Just Platform.Haskell

    Hspec.it "sets the level" $ do
      event <- Event.new
      Event.level event `Hspec.shouldBe` Just Level.Error

    Hspec.it "sets the environment" $ do
      event <- Event.new
      Event.environment event `Hspec.shouldBe` Text.pack "production"

    Hspec.it "sets the version" $ do
      event <- Event.new
      Event.version event `Hspec.shouldBe` Constant.sentryVersion

    Hspec.it "sets the type" $ do
      event <- Event.new
      Event.type_ event `Hspec.shouldBe` Just EventType.Default

  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let event = Event.empty
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a breadcrumb" $ do
      let breadcrumb = Breadcrumb.empty {Breadcrumb.category = Text.pack "example-category"}
          breadcrumbs = Breadcrumbs.empty {Breadcrumbs.values = [breadcrumb]}
          event = Event.empty {Event.breadcrumbs = Just breadcrumbs}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "breadcrumbs": { "values": [ { "category": "example-category" } ] } } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a context" $ do
      let context = Context.Other . Map.singleton (Text.pack "example-key") $ Aeson.Bool True
          contexts = Map.singleton (Text.pack "example-context") context
          event = Event.empty {Event.contexts = contexts}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "contexts": { "example-context": { "example-key": true } } } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with some debug meta" $ do
      let image = DebugImage.Other . Map.singleton (Text.pack "example-image") $ Aeson.Bool True
          debugMeta = DebugMeta.empty {DebugMeta.images = [image]}
          event = Event.empty {Event.debugMeta = Just debugMeta}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "debug_meta": { "images": [ { "example-image": true } ] } } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a dist" $ do
      let event = Event.empty {Event.dist = Text.pack "example-dist"}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "dist": "example-dist" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with an environment" $ do
      let event = Event.empty {Event.environment = Text.pack "example-environment"}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "environment": "example-environment" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with an event processing error" $ do
      let eventProcessingError = EventProcessingError.empty
          event = Event.empty {Event.errors = [eventProcessingError]}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "errors": [ { "type": "unknown_error" } ] } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with an exception" $ do
      let exception = Exception.empty {Exception.type_ = Text.pack "example-type"}
          exceptions = Exceptions.empty {Exceptions.values = [exception]}
          event = Event.empty {Event.exception = Just exceptions}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "exception": { "values": [ { "type": "example-type" } ] } } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with some extra" $ do
      let event = Event.empty {Event.extra = Map.singleton (Text.pack "example-extra") (Aeson.Bool True)}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "extra": { "example-extra": true } } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a fingerprint" $ do
      let event = Event.empty {Event.fingerprint = [Text.pack "example-fingerprint"]}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "fingerprint": ["example-fingerprint"] } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a level" $ do
      let event = Event.empty {Event.level = Just Level.Error}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "level": "error" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a log entry" $ do
      let logEntry = LogEntry.empty {LogEntry.message = Text.pack "example-message"}
          event = Event.empty {Event.logentry = Just logEntry}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "logentry": { "message": "example-message" } } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a logger" $ do
      let event = Event.empty {Event.logger = Text.pack "example-logger"}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "logger": "example-logger" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with some modules" $ do
      let event = Event.empty {Event.modules = Map.fromList [(Text.pack "module-name", Text.pack "module-version")]}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "modules": { "module-name": "module-version" } } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a platform" $ do
      let event = Event.empty {Event.platform = Just Platform.Haskell}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "platform": "haskell" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a release" $ do
      let event = Event.empty {Event.release = Text.pack "example-release"}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "release": "example-release" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a request" $ do
      let request = Request.empty {Request.fragment = Text.pack "example-fragment"}
          event = Event.empty {Event.request = Just request}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "request": { "fragment": "example-fragment" } } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with an SDK" $ do
      let clientSdkInfo = ClientSdkInfo.empty {ClientSdkInfo.name = Text.pack "example-name"}
          event = Event.empty {Event.sdk = Just clientSdkInfo}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "sdk": { "name": "example-name" } } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a server name" $ do
      let event = Event.empty {Event.serverName = Text.pack "example-server-name"}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "server_name": "example-server-name" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with some tags" $ do
      let event = Event.empty {Event.tags = Map.fromList [(Text.pack "tag-key", Text.pack "tag-value")]}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "tags": { "tag-key": "tag-value" } } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a time spent" $ do
      let event = Event.empty {Event.timeSpent = Just 1.2}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "time_spent": 1.2 } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a timestamp" $ do
      let event = Event.empty {Event.timestamp = Just $ Time.UTCTime (Time.fromGregorian 1970 1 1) 0}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "timestamp": "1970-01-01T00:00:00Z" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a transaction" $ do
      let event = Event.empty {Event.transaction = Text.pack "example-transaction"}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "transaction": "example-transaction" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with some transaction info" $ do
      let transactionInfo = TransactionInfo.empty {TransactionInfo.original = Text.pack "example-original"}
          event = Event.empty {Event.transactionInfo = Just transactionInfo}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "transaction_info": { "original": "example-original" } } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a type" $ do
      let event = Event.empty {Event.type_ = Just EventType.Default}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "type": "default" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a user" $ do
      let user = User.empty {User.email = Text.pack "example-email"}
          event = Event.empty {Event.user = Just user}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "user": { "email": "example-email" } } |]
      Aeson.toJSON event `Hspec.shouldBe` json

    Hspec.it "works with a version" $ do
      let event = Event.empty {Event.version = Text.pack "example-version"}
          json = [Aeson.aesonQQ| { "event_id": "00000000000000000000000000000000", "version": "example-version" } |]
      Aeson.toJSON event `Hspec.shouldBe` json

  Hspec.describe "intoRequest" $ do
    Hspec.it "sets the method" $ do
      dsn <- Dsn.fromText "http://public-key@sentry.test/project-id"
      event <- Event.new
      request <- Event.intoRequest dsn event
      Client.method request `Hspec.shouldBe` Http.methodPost

    Hspec.it "sets the host" $ do
      dsn <- Dsn.fromText "http://public-key@sentry.test:8080/project-id"
      event <- Event.new
      request <- Event.intoRequest dsn event
      Client.host request `Hspec.shouldBe` Text.encodeUtf8 (Text.pack "sentry.test")

    Hspec.it "sets the port" $ do
      dsn <- Dsn.fromText "http://public-key@sentry.test:8080/project-id"
      event <- Event.new
      request <- Event.intoRequest dsn event
      Client.port request `Hspec.shouldBe` 8080

    Hspec.it "sets the path" $ do
      dsn <- Dsn.fromText "http://public-key@sentry.test/project-id"
      event <- Event.new
      request <- Event.intoRequest dsn event
      Client.path request `Hspec.shouldBe` Text.encodeUtf8 (Text.pack "/api/project-id/store/")

    Hspec.it "handles a custom path" $ do
      dsn <- Dsn.fromText "http://public-key@sentry.test/custom/project-id"
      event <- Event.new
      request <- Event.intoRequest dsn event
      Client.path request `Hspec.shouldBe` Text.encodeUtf8 (Text.pack "/custom/api/project-id/store/")

    Hspec.it "sets the body" $ do
      dsn <- Dsn.fromText "http://public-key@sentry.test/project-id"
      event <- Event.new
      request <- Event.intoRequest dsn event
      case Client.requestBody request of
        Client.RequestBodyBS byteString -> byteString `Hspec.shouldBe` LazyByteString.toStrict (Aeson.encode event)
        _ -> fail "unexpected request body"

    Hspec.it "sets the content type" $ do
      dsn <- Dsn.fromText "http://public-key@sentry.test/project-id"
      event <- Event.new
      request <- Event.intoRequest dsn event
      lookup Http.hContentType (Client.requestHeaders request) `Hspec.shouldSatisfy` Maybe.isJust

    Hspec.it "sets the user agent" $ do
      dsn <- Dsn.fromText "http://public-key@sentry.test/project-id"
      event <- Event.new
      request <- Event.intoRequest dsn event
      lookup Http.hUserAgent (Client.requestHeaders request) `Hspec.shouldSatisfy` Maybe.isJust

    Hspec.it "sets the authorization" $ do
      dsn <- Dsn.fromText "http://public-key@sentry.test/project-id"
      event <- Event.new
      request <- Event.intoRequest dsn event
      lookup Constant.xSentryAuth (Client.requestHeaders request) `Hspec.shouldSatisfy` Maybe.isJust

  Hspec.describe "fromSomeException" $ do
    Hspec.it "sets the environment" $ do
      event <- Event.fromSomeException . Catch.toException $ userError ""
      Event.environment event `Hspec.shouldBe` Text.pack "production"

    Hspec.it "sets the event ID" $ do
      event <- Event.fromSomeException . Catch.toException $ userError ""
      Event.eventId event `Hspec.shouldNotBe` EventId.empty

    Hspec.it "sets the exception" $ do
      event <- Event.fromSomeException . Catch.toException $ userError ""
      Event.exception event `Hspec.shouldSatisfy` Maybe.isJust

    Hspec.it "sets the level" $ do
      event <- Event.fromSomeException . Catch.toException $ userError ""
      Event.level event `Hspec.shouldBe` Just Level.Error

    Hspec.it "sets the platform" $ do
      event <- Event.fromSomeException . Catch.toException $ userError ""
      Event.platform event `Hspec.shouldBe` Just Platform.Haskell

    Hspec.it "sets the timestamp" $ do
      event <- Event.fromSomeException . Catch.toException $ userError ""
      Event.timestamp event `Hspec.shouldSatisfy` Maybe.isJust

    Hspec.it "sets the type" $ do
      event <- Event.fromSomeException . Catch.toException $ userError ""
      Event.type_ event `Hspec.shouldBe` Just EventType.Default

    Hspec.it "sets the version" $ do
      event <- Event.fromSomeException . Catch.toException $ userError ""
      Event.version event `Hspec.shouldBe` Constant.sentryVersion
