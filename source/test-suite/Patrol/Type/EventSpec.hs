{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.EventSpec where

import qualified Data.Aeson as Aeson
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
import qualified Patrol.Type.Event.Id as Event.Id
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Event" $ do
  Hspec.describe "new" $ do
    Hspec.it "generates an ID" $ do
      event <- Event.new
      Event.id event `Hspec.shouldNotBe` Event.Id.fromUuid Uuid.nil

  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let event = Event.Event {Event.id = Event.Id.fromUuid Uuid.nil}
          lazyByteString = LazyByteString.fromStrict . Text.encodeUtf8 $ Text.pack "{\"event_id\":\"00000000000000000000000000000000\"}"
      Aeson.encode event `Hspec.shouldBe` lazyByteString

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