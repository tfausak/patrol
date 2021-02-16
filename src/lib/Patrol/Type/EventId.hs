module Patrol.Type.EventId
  ( EventId
  , fromUuid
  , toUuid
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.UUID as Uuid

-- | <https://develop.sentry.dev/sdk/event-payloads/#required-attributes>
newtype EventId
  = EventId Uuid.UUID
  deriving (Eq, Show)

instance Aeson.FromJSON EventId where
  parseJSON = Aeson.withText "EventId" $ \ text -> case Text.chunksOf 4 text of
    [a, b, c, d, e, f, g, h] -> maybe (fail "invalid EventId") (pure . fromUuid)
      . Uuid.fromText
      $ Text.intercalate (Text.singleton '-') [a <> b, c, d, e, f <> g <> h]
    _ -> fail "invalid EventId"

instance Aeson.ToJSON EventId where
  toJSON = Aeson.toJSON . Text.filter (/= '-') . Uuid.toText . toUuid

fromUuid :: Uuid.UUID -> EventId
fromUuid = EventId

toUuid :: EventId -> Uuid.UUID
toUuid (EventId x) = x
