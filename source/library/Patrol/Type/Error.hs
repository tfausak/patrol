module Patrol.Type.Error where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.ErrorType as ErrorType

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#eventprocessingerror>
data Error = Error
  { name :: Maybe Text.Text,
    type_ :: ErrorType.ErrorType,
    value :: Aeson.Value
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Error where
  toJSON error_ =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "name" Aeson..= name error_,
          Key.fromString "type" Aeson..= type_ error_,
          Key.fromString "value" Aeson..= value error_
        ]
