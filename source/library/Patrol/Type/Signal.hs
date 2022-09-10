module Patrol.Type.Signal where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

data Signal = Signal
  { code :: Maybe Int,
    codeName :: Maybe Text.Text,
    name :: Maybe Text.Text,
    number :: Int
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Signal where
  toJSON signal =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "code" Aeson..= code signal,
          Key.fromString "code_name" Aeson..= codeName signal,
          Key.fromString "name" Aeson..= name signal,
          Key.fromString "number" Aeson..= number signal
        ]
