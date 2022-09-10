module Patrol.Type.Mechanism where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

data Mechanism = Mechanism
  { data_ :: Aeson.Value,
    description :: Maybe Text.Text,
    handled :: Maybe Bool,
    helpLink :: Maybe Text.Text,
    synthetic :: Maybe Bool,
    type_ :: Text.Text
    -- TODO: meta
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Mechanism where
  toJSON exception =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "data" Aeson..= data_ exception,
          Key.fromString "description" Aeson..= description exception,
          Key.fromString "handled" Aeson..= handled exception,
          Key.fromString "help_link" Aeson..= helpLink exception,
          Key.fromString "synthetic" Aeson..= synthetic exception,
          Key.fromString "type" Aeson..= type_ exception
        ]
