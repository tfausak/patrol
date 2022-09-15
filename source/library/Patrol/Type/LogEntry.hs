module Patrol.Type.LogEntry where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#logentry>
data LogEntry = LogEntry
  { formatted :: Text.Text,
    message :: Text.Text,
    params :: Aeson.Value
  }
  deriving (Eq, Show)

instance Aeson.ToJSON LogEntry where
  toJSON logEntry =
    Aeson.intoObject
      [ Aeson.pair "formatted" $ formatted logEntry,
        Aeson.pair "message" $ message logEntry,
        Aeson.pair "params" $ params logEntry
      ]

empty :: LogEntry
empty =
  LogEntry
    { formatted = Text.empty,
      message = Text.empty,
      params = Aeson.Null
    }
