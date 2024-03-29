{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Patrol.Type.EventIdSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Data.UUID as Uuid
import qualified Data.UUID.V4 as Uuid
import qualified Patrol.Type.EventId as EventId
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.EventId" $ do
  Hspec.describe "fromUuid" $ do
    Hspec.it "converts from UUID" $ do
      uuid <- Uuid.nextRandom
      EventId.fromUuid uuid `Hspec.shouldBe` EventId.EventId uuid

  Hspec.describe "intoUuid" $ do
    Hspec.it "converts into UUID" $ do
      uuid <- Uuid.nextRandom
      EventId.intoUuid (EventId.fromUuid uuid) `Hspec.shouldBe` uuid

  Hspec.describe "random" $ do
    Hspec.it "generates a non-nil UUID" $ do
      eventId <- EventId.random
      eventId `Hspec.shouldNotBe` EventId.empty

  Hspec.describe "intoText" $ do
    Hspec.it "converts into text" $ do
      let eventId = EventId.fromUuid $ Uuid.fromWords64 0x0011223344556677 0x8899aabbccddeeff
      EventId.intoText eventId `Hspec.shouldBe` Text.pack "00112233445566778899aabbccddeeff"

  Hspec.describe "fromText" $ do
    Hspec.it "converts from text" $ do
      let eventId = EventId.fromUuid $ Uuid.fromWords64 0x0011223344556677 0x8899aabbccddeeff
      EventId.fromText (Text.pack "00112233445566778899aabbccddeeff") `Hspec.shouldBe` Just eventId

    Hspec.it "succeeds with uppercase digits" $ do
      let eventId = EventId.fromUuid $ Uuid.fromWords64 0x0011223344556677 0x8899aabbccddeeff
      EventId.fromText (Text.pack "00112233445566778899AABBCCDDEEFF") `Hspec.shouldBe` Just eventId

    Hspec.it "fails with not enough digits" $ do
      EventId.fromText (Text.pack "0123456789abcdef0123456789abcde") `Hspec.shouldBe` Nothing

    Hspec.it "fails with too many digits" $ do
      EventId.fromText (Text.pack "0123456789abcdef0123456789abcdef0") `Hspec.shouldBe` Nothing

    Hspec.it "fails with invalid hexadecimal" $ do
      EventId.fromText (Text.pack "Z123456789abcdef0123456789abcdef") `Hspec.shouldBe` Nothing
      EventId.fromText (Text.pack "0123456789Zbcdef0123456789abcdef") `Hspec.shouldBe` Nothing
      EventId.fromText (Text.pack "0123456789abcdef0123456789Zbcdef") `Hspec.shouldBe` Nothing

  Hspec.describe "FromJSON" $ do
    Hspec.it "works" $ do
      let json = [Aeson.aesonQQ| "00112233445566778899aabbccddeeff" |]
          eventId = EventId.fromUuid $ Uuid.fromWords64 0x0011223344556677 0x8899aabbccddeeff
      Aeson.fromJSON json `Hspec.shouldBe` Aeson.Success eventId

    Hspec.it "fails with not enough digits" $ do
      let json = [Aeson.aesonQQ| "" |]
      Aeson.fromJSON @EventId.EventId json `Hspec.shouldBe` Aeson.Error "invalid EventId"

    Hspec.it "fails with the wrong type" $ do
      let json = [Aeson.aesonQQ| null |]
      Aeson.fromJSON @EventId.EventId json `Hspec.shouldBe` Aeson.Error "parsing EventId failed, expected String, but encountered Null"

  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let eventId = EventId.fromUuid $ Uuid.fromWords64 0x0011223344556677 0x8899aabbccddeeff
          json = [Aeson.aesonQQ| "00112233445566778899aabbccddeeff" |]
      Aeson.toJSON eventId `Hspec.shouldBe` json
