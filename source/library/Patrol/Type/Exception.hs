module Patrol.Type.Exception where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.ValueClass as ValueClass

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#exception>
newtype Exception = Exception
  { values :: [ValueClass.ValueClass]
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Exception where
  toJSON exception =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "values" Aeson..= values exception
        ]
