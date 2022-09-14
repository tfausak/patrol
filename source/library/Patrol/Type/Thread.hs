module Patrol.Type.Thread where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Stacktrace as Stacktrace

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#thread>
data Thread = Thread
  { crashed :: Maybe Bool,
    current :: Maybe Bool,
    id :: Maybe Text.Text,
    name :: Maybe Text.Text,
    stacktrace :: Maybe Stacktrace.Stacktrace
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Thread where
  toJSON thread =
    Aeson.intoObject
      [ Aeson.pair "crashed" $ crashed thread,
        Aeson.pair "current" $ current thread,
        Aeson.pair "id" $ Patrol.Type.Thread.id thread,
        Aeson.pair "name" $ name thread,
        Aeson.pair "stacktrace" $ stacktrace thread
      ]

empty :: Thread
empty =
  Thread
    { crashed = Nothing,
      current = Nothing,
      Patrol.Type.Thread.id = Nothing,
      name = Nothing,
      stacktrace = Nothing
    }
