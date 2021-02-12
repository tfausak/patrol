module Patrol.Type.Event
  ( Event(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Platform as Platform
import qualified Patrol.Type.Timestamp as Timestamp

data Event = Event
  { eventId :: EventId.EventId
  , platform :: Platform.Platform
  , timestamp :: Timestamp.Timestamp
  } deriving (Eq, Show)

instance Aeson.ToJSON Event where
  toJSON event = Aeson.object
    [ pair "event_id" $ eventId event
    , pair "platform" $ platform event
    , pair "timestamp" $ timestamp event
    ]

pair :: (Aeson.ToJSON value, Aeson.KeyValue pair) => String -> value -> pair
pair k v = Text.pack k Aeson..= v
