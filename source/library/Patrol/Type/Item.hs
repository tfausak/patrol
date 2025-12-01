{-# LANGUAGE OverloadedStrings #-}

module Patrol.Type.Item where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Patrol.Type.Event as Event

-- | <https://develop.sentry.dev/sdk/data-model/envelope-items/>
data Item
  = -- | An 'Patrol.Type.Event.Event' item.
    --
    -- <https://develop.sentry.dev/sdk/envelopes/#event>
    Event Event.Event
  | -- | A sentinel item used to filter raw envelopes.
    Raw
  deriving (Eq, Show)

serialize :: Item -> Builder.Builder
serialize item = case item of
  Event event ->
    let payload = LazyByteString.toStrict $ Aeson.encode event
        headers = buildHeaders "event" (ByteString.length payload)
     in headers <> Builder.char7 '\n' <> Builder.byteString payload
  Raw -> mempty
  where
    buildHeaders :: Key.Key -> Int -> Builder.Builder
    buildHeaders type_ length_ =
      Aeson.fromEncoding $
        Aeson.pairs ("type" Aeson..= type_ <> "length" Aeson..= length_)
