module Patrol.Type.StackTrace where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Frame as Frame

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#stacktrace>
data StackTrace = StackTrace
  { frames :: [Frame.Frame],
    registers :: Map.Map Text.Text Aeson.Value
  }
  deriving (Eq, Show)

instance Aeson.ToJSON StackTrace where
  toJSON stackTrace =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "frames" Aeson..= frames stackTrace,
          Key.fromString "registers" Aeson..= registers stackTrace
        ]
