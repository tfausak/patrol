module Patrol.Type.Mechanism where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Meta as Meta

data Mechanism = Mechanism
  { data_ :: Aeson.Value,
    description :: Maybe Text.Text,
    handled :: Maybe Bool,
    helpLink :: Maybe Text.Text,
    meta :: Maybe Meta.Meta,
    synthetic :: Maybe Bool,
    type_ :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Mechanism where
  toJSON mechanism =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "data" Aeson..= data_ mechanism,
          Key.fromString "description" Aeson..= description mechanism,
          Key.fromString "handled" Aeson..= handled mechanism,
          Key.fromString "help_link" Aeson..= helpLink mechanism,
          Key.fromString "meta" Aeson..= meta mechanism,
          Key.fromString "synthetic" Aeson..= synthetic mechanism,
          Key.fromString "type" Aeson..= type_ mechanism
        ]
