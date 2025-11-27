module Patrol.Type.Headers where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as Builder
import qualified Data.Time as Time
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.ClientSdkInfo as ClientSdkInfo
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.EventId as EventId

-- | <https://develop.sentry.dev/sdk/data-model/envelopes/#headers>
data Headers = Headers
  { eventId :: Maybe EventId.EventId,
    dsn :: Maybe Dsn.Dsn,
    sdk :: Maybe ClientSdkInfo.ClientSdkInfo,
    sentAt :: Maybe Time.UTCTime
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Headers where
  toJSON headers =
    Aeson.intoObject
      [ Aeson.pair "event_id" $ eventId headers,
        Aeson.pair "dsn" $ fmap Dsn.intoUri $ dsn headers,
        Aeson.pair "sdk" $ sdk headers,
        Aeson.pair "sent_at" $ sentAt headers
      ]

empty :: Headers
empty =
  Headers
    { eventId = Nothing,
      dsn = Nothing,
      sdk = Nothing,
      sentAt = Nothing
    }

serialize :: Headers -> Builder.Builder
serialize = Aeson.fromEncoding . Aeson.toEncoding
