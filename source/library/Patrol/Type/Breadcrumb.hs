module Patrol.Type.Breadcrumb where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.BreadcrumbType as BreadcrumbType
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Level as Level

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#breadcrumb>
data Breadcrumb = Breadcrumb
  { category :: Maybe Text.Text,
    data_ :: Map.Map Text.Text Aeson.Value,
    eventId :: Maybe EventId.EventId,
    level :: Maybe Level.Level,
    message :: Maybe Text.Text,
    timestamp :: Maybe Time.UTCTime,
    type_ :: Maybe BreadcrumbType.BreadcrumbType
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Breadcrumb where
  toJSON breadcrumb =
    Aeson.intoObject
      [ Aeson.pair "category" $ category breadcrumb,
        Aeson.pair "data" $ data_ breadcrumb,
        Aeson.pair "event_id" $ eventId breadcrumb,
        Aeson.pair "level" $ level breadcrumb,
        Aeson.pair "message" $ message breadcrumb,
        Aeson.pair "timestamp" $ timestamp breadcrumb,
        Aeson.pair "type" $ type_ breadcrumb
      ]

empty :: Breadcrumb
empty =
  Breadcrumb
    { category = Nothing,
      data_ = Map.empty,
      eventId = Nothing,
      level = Nothing,
      message = Nothing,
      timestamp = Nothing,
      type_ = Nothing
    }
