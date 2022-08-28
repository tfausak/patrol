module Patrol.Type.Timestamp
  ( Timestamp,
    fromUtcTime,
    toUtcTime,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Time as Time

-- | <https://develop.sentry.dev/sdk/event-payloads/#required-attributes>
newtype Timestamp
  = Timestamp Time.UTCTime
  deriving (Eq, Show)

instance Aeson.ToJSON Timestamp where
  toJSON = Aeson.toJSON . Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . toUtcTime

fromUtcTime :: Time.UTCTime -> Timestamp
fromUtcTime = Timestamp

toUtcTime :: Timestamp -> Time.UTCTime
toUtcTime (Timestamp x) = x
