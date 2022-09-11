module Patrol.Type.EventProcessingError where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.ErrorType as ErrorType

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#eventprocessingerror>
data EventProcessingError = EventProcessingError
  { name :: Maybe Text.Text,
    type_ :: ErrorType.ErrorType,
    value :: Aeson.Value
  }
  deriving (Eq, Show)

instance Aeson.ToJSON EventProcessingError where
  toJSON eventProcessingError =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Aeson.pair "name" $ name eventProcessingError,
          Aeson.pair "type" $ type_ eventProcessingError,
          Aeson.pair "value" $ value eventProcessingError
        ]
