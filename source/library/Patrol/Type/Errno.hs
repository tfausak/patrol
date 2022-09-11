module Patrol.Type.Errno where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#cerror>
data Errno = Errno
  { name :: Maybe Text.Text,
    number :: Maybe Int
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Errno where
  toJSON nsError =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "name" Aeson..= name nsError,
          Key.fromString "number" Aeson..= number nsError
        ]
