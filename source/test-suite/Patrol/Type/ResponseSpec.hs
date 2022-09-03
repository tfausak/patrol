module Patrol.Type.ResponseSpec where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as Uuid
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Response as Response
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Response" $ do
  Hspec.describe "FromJSON" $ do
    Hspec.it "works" $ do
      let lazyByteString = LazyByteString.fromStrict . Text.encodeUtf8 $ Text.pack "{\"id\":\"00112233445566778899aabbccddeeff\"}"
          eventId = EventId.fromUuid $ Uuid.fromWords64 0x0011223344556677 0x8899aabbccddeeff
          response = Response.Response {Response.id = eventId}
      Aeson.decode lazyByteString `Hspec.shouldBe` Just response

    Hspec.it "fails with the wrong type" $ do
      let lazyByteString = LazyByteString.fromStrict . Text.encodeUtf8 $ Text.pack "null"
      Aeson.eitherDecode lazyByteString `Hspec.shouldBe` (Left "Error in $: parsing Response failed, expected Object, but encountered Null" :: Either String Response.Response)
