module Patrol.Type.Event
  ( Event(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Platform as Platform
import qualified Patrol.Type.Timestamp as Timestamp
import qualified Patrol.Utility.Json as Json

data Event = Event
  { eventId :: EventId.EventId
  , platform :: Platform.Platform
  , timestamp :: Timestamp.Timestamp
  } deriving (Eq, Show)

instance Aeson.ToJSON Event where
  toJSON event = Aeson.object
    [ Json.pair "event_id" $ eventId event
    , Json.pair "platform" $ platform event
    , Json.pair "timestamp" $ timestamp event
    ]
