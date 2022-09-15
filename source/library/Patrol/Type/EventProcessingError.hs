module Patrol.Type.EventProcessingError where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.ErrorType as ErrorType

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#eventprocessingerror>
data EventProcessingError = EventProcessingError
  { name :: Text.Text,
    type_ :: ErrorType.ErrorType,
    value :: Aeson.Value
  }
  deriving (Eq, Show)

instance Aeson.ToJSON EventProcessingError where
  toJSON eventProcessingError =
    Aeson.intoObject
      [ Aeson.pair "name" $ name eventProcessingError,
        Aeson.pair "type" $ type_ eventProcessingError,
        Aeson.pair "value" $ value eventProcessingError
      ]

empty :: EventProcessingError
empty =
  EventProcessingError
    { name = Text.empty,
      type_ = ErrorType.UnknownError,
      value = Aeson.Null
    }
