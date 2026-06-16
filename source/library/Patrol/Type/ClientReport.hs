module Patrol.Type.ClientReport where

import qualified Data.Aeson as Aeson
import qualified Data.Time as Time
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.DiscardedEvent as DiscardedEvent

-- | A client report envelope item, used to report locally-discarded telemetry
-- back to Sentry.
--
-- <https://develop.sentry.dev/sdk/telemetry/client-reports/>
data ClientReport = ClientReport
  { timestamp :: Maybe Time.UTCTime,
    discardedEvents :: [DiscardedEvent.DiscardedEvent]
  }
  deriving (Eq, Show)

instance Aeson.ToJSON ClientReport where
  toJSON clientReport =
    Aeson.intoObject
      [ Aeson.pair "timestamp" $ timestamp clientReport,
        Aeson.pair "discarded_events" $ discardedEvents clientReport
      ]

empty :: ClientReport
empty =
  ClientReport
    { timestamp = Nothing,
      discardedEvents = []
    }
