module Patrol.Type.MachException where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#machexception>
data MachException = MachException
  { code :: Maybe Int,
    exception :: Maybe Int,
    name :: Maybe Text.Text,
    subcode :: Maybe Int
  }
  deriving (Eq, Show)

instance Aeson.ToJSON MachException where
  toJSON machException =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "code" Aeson..= code machException,
          Key.fromString "exception" Aeson..= exception machException,
          Key.fromString "name" Aeson..= name machException,
          Key.fromString "subcode" Aeson..= subcode machException
        ]
