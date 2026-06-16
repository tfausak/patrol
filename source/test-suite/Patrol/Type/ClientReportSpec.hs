{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ClientReportSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Patrol.Type.ClientReport as ClientReport
import qualified Patrol.Type.DataCategory as DataCategory
import qualified Patrol.Type.DiscardedEvent as DiscardedEvent
import qualified Patrol.Type.Item as Item
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.ClientReport" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "omits timestamp when absent" $ do
      let report =
            ClientReport.ClientReport
              { ClientReport.timestamp = Nothing,
                ClientReport.discardedEvents =
                  [ DiscardedEvent.DiscardedEvent
                      { DiscardedEvent.reason = "sample_rate",
                        DiscardedEvent.category = DataCategory.Error,
                        DiscardedEvent.quantity = 3
                      }
                  ]
              }
      Aeson.toJSON report
        `Hspec.shouldBe` [Aeson.aesonQQ| {"discarded_events": [{"reason": "sample_rate", "category": "error", "quantity": 3}]} |]

    Hspec.it "omits discarded_events when empty" $ do
      Aeson.toJSON ClientReport.empty
        `Hspec.shouldBe` [Aeson.aesonQQ| {} |]

  Hspec.describe "Item.serialize" $ do
    Hspec.it "frames a ClientReport item with the correct type header" $ do
      let report =
            ClientReport.ClientReport
              { ClientReport.timestamp = Nothing,
                ClientReport.discardedEvents =
                  [ DiscardedEvent.DiscardedEvent
                      { DiscardedEvent.reason = "sample_rate",
                        DiscardedEvent.category = DataCategory.Error,
                        DiscardedEvent.quantity = 3
                      }
                  ]
              }
          actual =
            LazyByteString.toStrict
              . Builder.toLazyByteString
              . Item.serialize
              $ Item.ClientReport report
          -- The serialized item is a one-line JSON header, a newline, then the
          -- payload. aeson emits no newlines, so the separator inserted by
          -- 'Item.serialize' is the only one and splitting on it is safe.
          (header, rest) = Char8.break (== '\n') actual
          payload = ByteString.drop 1 rest
      -- Assert the header framing and that it declares the payload's length.
      Aeson.decodeStrict header
        `Hspec.shouldBe` Just
          ( Aeson.object
              [ "type" Aeson..= ("client_report" :: String),
                "length" Aeson..= ByteString.length payload
              ]
          )
      -- Compare the payload as decoded JSON so the assertion does not depend on
      -- aeson's (semantically meaningless) object key ordering.
      Aeson.decodeStrict payload
        `Hspec.shouldBe` Just [Aeson.aesonQQ| {"discarded_events": [{"reason": "sample_rate", "category": "error", "quantity": 3}]} |]
