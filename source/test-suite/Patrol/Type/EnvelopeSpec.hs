{-# LANGUAGE OverloadedStrings #-}

module Patrol.Type.EnvelopeSpec where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Patrol.Constant as Constant
import qualified Patrol.Type.ClientSdkInfo as ClientSdkInfo
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.Envelope as Envelope
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Event as Event
import qualified Patrol.Type.Headers as Headers
import qualified Patrol.Type.Item as Item
import qualified Patrol.Type.Items as Items
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Envelope" $ do
  Hspec.describe "serialize" $ do
    Hspec.it "works with no items" $ do
      let actual =
            Builder.toLazyByteString $
              Envelope.serialize
                Envelope.Envelope
                  { Envelope.headers = Headers.empty,
                    Envelope.items = Items.EnvelopeItems []
                  }
      actual `Hspec.shouldBe` "{}\n"

    Hspec.it "works with 'Items.Raw'" $ do
      let actual =
            Builder.toLazyByteString $
              Envelope.serialize
                Envelope.Envelope
                  { Envelope.headers = Headers.empty,
                    Envelope.items = Items.Raw ""
                  }
      actual `Hspec.shouldBe` "{}\n"

    Hspec.it "works with 'Items.EnvelopeItems'" $ do
      let actual =
            Builder.toLazyByteString $
              Envelope.serialize Envelope.Envelope
                { Envelope.headers = Headers.empty,
                  Envelope.items = Items.EnvelopeItems [Item.Event Event.empty]
                }
      actual `Hspec.shouldBe` "{}\n{\"type\":\"event\",\"length\":47}\n{\"event_id\":\"00000000000000000000000000000000\"}\n"

  Hspec.describe "fromEvent" $ do
    Hspec.it "sets the header" $ do
      dsn <- Dsn.fromText "http://key@sentry.test/1"
      let envelope = Envelope.fromEvent dsn Event.empty
      let expected =
            Headers.Headers
              { Headers.eventId = Just EventId.empty,
                Headers.dsn = Just dsn,
                Headers.sdk = Just ClientSdkInfo.patrol,
                Headers.sentAt = Event.timestamp Event.empty
              }
      Envelope.headers envelope `Hspec.shouldBe` expected

    Hspec.it "sets the items" $ do
      dsn <- Dsn.fromText "http://key@sentry.test/1"
      let envelope = Envelope.fromEvent dsn Event.empty
      case Envelope.items envelope of
        Items.EnvelopeItems items ->
          items `Hspec.shouldBe` [Item.Event Event.empty]
        other -> Hspec.expectationFailure $ "expected an envelope containing an event, got: " <> show other

  Hspec.describe "intoRequest" $ do
    Hspec.it "sets the method" $ do
      dsn <- Dsn.fromText "http://key@sentry.test/1"
      let envelope = Envelope.fromEvent dsn Event.empty
      request <- Envelope.intoRequest dsn envelope
      Client.method request `Hspec.shouldBe` Http.methodPost

    Hspec.it "sets the host" $ do
      dsn <- Dsn.fromText "http://key@sentry.test/1"
      let envelope = Envelope.fromEvent dsn Event.empty
      request <- Envelope.intoRequest dsn envelope
      Client.host request `Hspec.shouldBe` "sentry.test"

    Hspec.it "sets the port" $ do
      dsn <- Dsn.fromText "http://key@sentry.test:8080/1"
      let envelope = Envelope.fromEvent dsn Event.empty
      request <- Envelope.intoRequest dsn envelope
      Client.port request `Hspec.shouldBe` 8080

    Hspec.it "sets the path" $ do
      dsn <- Dsn.fromText "http://key@sentry.test/1"
      let envelope = Envelope.fromEvent dsn Event.empty
      request <- Envelope.intoRequest dsn envelope
      Client.path request `Hspec.shouldBe` "/api/1/envelope/"

    Hspec.it "handles a custom path" $ do
      dsn <- Dsn.fromText "http://key@sentry.test/custom/1"
      let envelope = Envelope.fromEvent dsn Event.empty
      request <- Envelope.intoRequest dsn envelope
      Client.path request `Hspec.shouldBe` "/custom/api/1/envelope/"

    Hspec.it "sets the body" $ do
      dsn <- Dsn.fromText "http://key@sentry.test/1"
      let envelope = Envelope.fromEvent dsn Event.empty
      request <- Envelope.intoRequest dsn envelope
      actual <- case Client.requestBody request of
        Client.RequestBodyBS byteString -> pure byteString
        _ -> fail "unexpected request body"
      let expected = LazyByteString.toStrict . Builder.toLazyByteString $ Envelope.serialize envelope
      actual `Hspec.shouldBe` expected

    Hspec.it "sets the content type" $ do
      dsn <- Dsn.fromText "http://key@sentry.test/1"
      let envelope = Envelope.fromEvent dsn Event.empty
      request <- Envelope.intoRequest dsn envelope
      lookup Http.hContentType (Client.requestHeaders request) `Hspec.shouldBe` Just Constant.applicationXSentryEnvelope

    Hspec.it "sets the user agent" $ do
      dsn <- Dsn.fromText "http://key@sentry.test/1"
      let envelope = Envelope.fromEvent dsn Event.empty
      request <- Envelope.intoRequest dsn envelope
      lookup Http.hUserAgent (Client.requestHeaders request) `Hspec.shouldBe` Just (Text.encodeUtf8 Constant.userAgent)

    Hspec.it "sets the authorization" $ do
      dsn <- Dsn.fromText "http://key@sentry.test/1"
      let envelope = Envelope.fromEvent dsn Event.empty
      request <- Envelope.intoRequest dsn envelope
      lookup Constant.xSentryAuth (Client.requestHeaders request) `Hspec.shouldBe` Just (Dsn.intoAuthorization dsn)
