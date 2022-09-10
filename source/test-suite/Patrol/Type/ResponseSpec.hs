{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Patrol.Type.ResponseSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.UUID as Uuid
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Response as Response
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Response" $ do
  Hspec.describe "FromJSON" $ do
    Hspec.it "works" $ do
      let json = [Aeson.aesonQQ| { "id": "00112233445566778899aabbccddeeff" } |]
          eventId = EventId.fromUuid $ Uuid.fromWords64 0x0011223344556677 0x8899aabbccddeeff
          response = Response.Response {Response.id = eventId}
      Aeson.fromJSON json `Hspec.shouldBe` Aeson.Success response

    Hspec.it "fails with the wrong type" $ do
      let json = [Aeson.aesonQQ| null |]
      Aeson.fromJSON @Response.Response json `Hspec.shouldBe` Aeson.Error "parsing Response failed, expected Object, but encountered Null"
