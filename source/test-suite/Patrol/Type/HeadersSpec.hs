{-# LANGUAGE OverloadedStrings #-}

module Patrol.Type.HeadersSpec where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.UUID as Uuid
import qualified Patrol.Type.ClientSdkInfo as ClientSdkInfo
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Headers as Headers
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Headers" $ do
  Hspec.describe "serialize" $ do
    Hspec.it "works with empty Headers" $ do
      let actual = Builder.toLazyByteString $ Headers.serialize Headers.empty
      actual `Hspec.shouldBe` "{}"

    Hspec.it "works with an EventId" $ do
      let actual =
            Builder.toLazyByteString
              . Headers.serialize
              $ Headers.empty {Headers.eventId = Just $ EventId.fromUuid Uuid.nil}
      actual `Hspec.shouldBe` "{\"event_id\":\"00000000000000000000000000000000\"}"

    Hspec.it "works with a DSN" $ do
      dsn <- Dsn.fromText (Text.pack "a://b@c/d")
      let actual =
            Builder.toLazyByteString
              . Headers.serialize
              $ Headers.empty {Headers.dsn = Just dsn}
      actual `Hspec.shouldBe` "{\"dsn\":\"a://b@c/d\"}"

    Hspec.it "works with ClientSdkInfo" $ do
      let clientSdkInfo = ClientSdkInfo.empty {ClientSdkInfo.name = "patrol"}
      let actual =
            Builder.toLazyByteString
              . Headers.serialize
              $ Headers.empty {Headers.sdk = Just clientSdkInfo}
      actual `Hspec.shouldBe` "{\"sdk\":{\"name\":\"patrol\"}}"

    Hspec.it "works with a 'sentAt' timestamp" $ do
      let timestamp = Time.UTCTime (Time.fromGregorian 1970 1 1) 0
      let actual =
            Builder.toLazyByteString
              . Headers.serialize
              $ Headers.empty {Headers.sentAt = Just timestamp}
      actual `Hspec.shouldBe` "{\"sent_at\":\"1970-01-01T00:00:00Z\"}"
