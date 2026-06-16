module Patrol.Type.DiscardedEvent where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.DataCategory as DataCategory

-- | A single entry in a client report describing how many events were discarded
-- for a given reason and data category.
--
-- <https://develop.sentry.dev/sdk/telemetry/client-reports/#envelope-item-payload>
data DiscardedEvent = DiscardedEvent
  { reason :: Text.Text,
    category :: DataCategory.DataCategory,
    quantity :: Int
  }
  deriving (Eq, Show)

instance Aeson.ToJSON DiscardedEvent where
  toJSON discardedEvent =
    Aeson.intoObject
      [ Aeson.pair "reason" $ reason discardedEvent,
        Aeson.pair "category" $ category discardedEvent,
        Aeson.pair "quantity" $ quantity discardedEvent
      ]
