module Patrol.Type.Mechanism where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.MechanismMeta as MechanismMeta

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#mechanism>
data Mechanism = Mechanism
  { data_ :: Map.Map Text.Text Aeson.Value,
    description :: Text.Text,
    handled :: Maybe Bool,
    helpLink :: Text.Text,
    meta :: Maybe MechanismMeta.MechanismMeta,
    synthetic :: Maybe Bool,
    type_ :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Mechanism where
  toJSON mechanism =
    Aeson.intoObject
      [ Aeson.pair "data" $ data_ mechanism,
        Aeson.pair "description" $ description mechanism,
        Aeson.pair "handled" $ handled mechanism,
        Aeson.pair "help_link" $ helpLink mechanism,
        Aeson.pair "meta" $ meta mechanism,
        Aeson.pair "synthetic" $ synthetic mechanism,
        Aeson.pair "type" $ type_ mechanism
      ]

empty :: Mechanism
empty =
  Mechanism
    { data_ = Map.empty,
      description = Text.empty,
      handled = Nothing,
      helpLink = Text.empty,
      meta = Nothing,
      synthetic = Nothing,
      type_ = Text.empty
    }
