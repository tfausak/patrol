module Patrol.Type.Event.IdSpec where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as Uuid
import qualified Patrol.Type.Event.Id as Event.Id
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Event.Id" $ do
  Hspec.describe "fromUuid" $ do
    Hspec.it "converts from UUID" $ do
      let uuid = Uuid.nil
      Event.Id.fromUuid Uuid.nil `Hspec.shouldBe` Event.Id.Id uuid

  Hspec.describe "intoUuid" $ do
    Hspec.it "converts into UUID" $ do
      let uuid = Uuid.nil
      Event.Id.intoUuid (Event.Id.fromUuid uuid) `Hspec.shouldBe` uuid

  Hspec.describe "random" $ do
    Hspec.it "generates a non-nil UUID" $ do
      eventId <- Event.Id.random
      eventId `Hspec.shouldNotBe` Event.Id.fromUuid Uuid.nil

  Hspec.describe "intoText" $ do
    Hspec.it "converts into text" $ do
      let eventId = Event.Id.fromUuid $ Uuid.fromWords64 0x0011223344556677 0x8899aabbccddeeff
      Event.Id.intoText eventId `Hspec.shouldBe` Text.pack "00112233445566778899aabbccddeeff"

  Hspec.describe "fromText" $ do
    Hspec.it "converts from text" $ do
      let eventId = Event.Id.fromUuid $ Uuid.fromWords64 0x0011223344556677 0x8899aabbccddeeff
      Event.Id.fromText (Text.pack "00112233445566778899aabbccddeeff") `Hspec.shouldBe` Just eventId

    Hspec.it "succeeds with uppercase digits" $ do
      let eventId = Event.Id.fromUuid $ Uuid.fromWords64 0x0011223344556677 0x8899aabbccddeeff
      Event.Id.fromText (Text.pack "00112233445566778899AABBCCDDEEFF") `Hspec.shouldBe` Just eventId

    Hspec.it "fails with not enough digits" $ do
      Event.Id.fromText (Text.pack "0123456789abcdef0123456789abcde") `Hspec.shouldBe` Nothing

    Hspec.it "fails with too many digits" $ do
      Event.Id.fromText (Text.pack "0123456789abcdef0123456789abcdef0") `Hspec.shouldBe` Nothing

    Hspec.it "fails with invalid hexadecimal" $ do
      Event.Id.fromText (Text.pack "0123456789Zbcdef0123456789abcdef") `Hspec.shouldBe` Nothing
      Event.Id.fromText (Text.pack "0123456789abcdef0123456789Zbcdef") `Hspec.shouldBe` Nothing

  Hspec.describe "FromJSON" $ do
    Hspec.it "works" $ do
      let lazyByteString = LazyByteString.fromStrict . Text.encodeUtf8 $ Text.pack "\"00112233445566778899aabbccddeeff\""
          eventId = Event.Id.fromUuid $ Uuid.fromWords64 0x0011223344556677 0x8899aabbccddeeff
      Aeson.decode lazyByteString `Hspec.shouldBe` Just eventId

    Hspec.it "fails with not enough digits" $ do
      let lazyByteString = LazyByteString.fromStrict . Text.encodeUtf8 $ Text.pack "\"\""
      Aeson.eitherDecode lazyByteString `Hspec.shouldBe` (Left "Error in $: invalid Id" :: Either String Event.Id.Id)

    Hspec.it "fails with the wrong type" $ do
      let lazyByteString = LazyByteString.fromStrict . Text.encodeUtf8 $ Text.pack "null"
      Aeson.eitherDecode lazyByteString `Hspec.shouldBe` (Left "Error in $: parsing Id failed, expected String, but encountered Null" :: Either String Event.Id.Id)

  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let eventId = Event.Id.fromUuid $ Uuid.fromWords64 0x0011223344556677 0x8899aabbccddeeff
          lazyByteString = LazyByteString.fromStrict . Text.encodeUtf8 $ Text.pack "\"00112233445566778899aabbccddeeff\""
      Aeson.encode eventId `Hspec.shouldBe` lazyByteString
