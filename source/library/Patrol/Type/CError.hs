module Patrol.Type.CError where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#cerror>
data CError = CError
  { name :: Maybe Text.Text,
    number :: Maybe Int
  }
  deriving (Eq, Show)

instance Aeson.ToJSON CError where
  toJSON cError =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Aeson.pair "name" $ name cError,
          Aeson.pair "number" $ number cError
        ]
