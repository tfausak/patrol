module Patrol.Type.NsError where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#nserror>
data NsError = NsError
  { code :: Maybe Int,
    domain :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON NsError where
  toJSON nsError =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "code" Aeson..= code nsError,
          Key.fromString "domain" Aeson..= domain nsError
        ]
