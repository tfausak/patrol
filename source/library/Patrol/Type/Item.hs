{-# LANGUAGE OverloadedStrings #-}

module Patrol.Type.Item where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Int (Int64)
import qualified Patrol.Type.ClientReport as ClientReport
import qualified Patrol.Type.Event as Event

-- | <https://develop.sentry.dev/sdk/data-model/envelope-items/>
data Item
  = -- | An 'Patrol.Type.Event.Event' item.
    --
    -- <https://develop.sentry.dev/sdk/envelopes/#event>
    Event Event.Event
  | -- | A 'Patrol.Type.ClientReport.ClientReport' item, used to report
    -- locally-discarded telemetry back to Sentry.
    --
    -- <https://develop.sentry.dev/sdk/telemetry/client-reports/>
    ClientReport ClientReport.ClientReport
  | -- | A sentinel item used to filter raw envelopes.
    Raw
  deriving (Eq, Show)

serialize :: Item -> Builder.Builder
serialize item = case item of
  Event event -> envelopeItem "event" (Aeson.encode event)
  ClientReport clientReport -> envelopeItem "client_report" (Aeson.encode clientReport)
  Raw -> mempty
  where
    -- Keep the encoded payload lazy: take the item-header length from the lazy
    -- chunks and stream them straight into the builder, rather than forcing a
    -- contiguous strict copy of every payload.
    envelopeItem :: Key.Key -> LazyByteString.ByteString -> Builder.Builder
    envelopeItem type_ payload =
      buildHeaders type_ (LazyByteString.length payload)
        <> Builder.char7 '\n'
        <> Builder.lazyByteString payload

    buildHeaders :: Key.Key -> Int64 -> Builder.Builder
    buildHeaders type_ length_ =
      Aeson.fromEncoding $
        Aeson.pairs ("type" Aeson..= type_ <> "length" Aeson..= length_)
