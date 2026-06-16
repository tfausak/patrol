{-# LANGUAGE OverloadedStrings #-}

module Patrol.Type.DataCategory where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Patrol.Exception.Problem as Problem

-- | The data category associated with discarded and/or rate-limited events.
--
-- <https://develop.sentry.dev/sdk/foundations/transport/rate-limiting/#definitions>
-- <https://develop.sentry.dev/sdk/telemetry/client-reports/#data-categories>
data DataCategory
  = Default
  | Error
  | Transaction
  | Monitor
  | Span
  | LogItem
  | Security
  | Attachment
  | Session
  | Profile
  | ProfileChunk
  | Replay
  | Feedback
  | TraceMetric
  | Internal
  deriving (Eq, Ord, Show)

instance Aeson.FromJSON DataCategory where
  parseJSON =
    let name = show $ Typeable.typeRep (Typeable.Proxy :: Typeable.Proxy DataCategory)
     in Aeson.withText name $ maybe (fail $ "invalid " <> name) pure . fromText

instance Aeson.ToJSON DataCategory where
  toJSON = Aeson.toJSON . intoText

-- | Render a 'DataCategory' to its canonical wire format.
intoText :: DataCategory -> Text.Text
intoText dataCategory = case dataCategory of
  Attachment -> "attachment"
  Default -> "default"
  Error -> "error"
  Feedback -> "feedback"
  Internal -> "internal"
  LogItem -> "log_item"
  Monitor -> "monitor"
  Profile -> "profile"
  ProfileChunk -> "profile_chunk"
  Replay -> "replay"
  Security -> "security"
  Session -> "session"
  Span -> "span"
  TraceMetric -> "trace_metric"
  Transaction -> "transaction"

-- | Attempt to parse the given string into a 'DataCategory'; the inverse of
-- 'intoText'.
fromText :: (Catch.MonadThrow m) => Text.Text -> m DataCategory
fromText text = case text of
  "attachment" -> pure Attachment
  "default" -> pure Default
  "error" -> pure Error
  "feedback" -> pure Feedback
  "internal" -> pure Internal
  "log_item" -> pure LogItem
  "monitor" -> pure Monitor
  "profile" -> pure Profile
  "profile_chunk" -> pure ProfileChunk
  "replay" -> pure Replay
  "security" -> pure Security
  "session" -> pure Session
  "span" -> pure Span
  "trace_metric" -> pure TraceMetric
  "transaction" -> pure Transaction
  _ -> Catch.throwM . Problem.Problem $ "invalid DataCategory: " <> show text
