module Patrol.Type.Error where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.ErrorType as ErrorType

data Error = Error
  { type_ :: ErrorType.ErrorType,
    value :: Map.Map Text.Text Aeson.Value
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Error where
  toJSON error_ =
    Aeson.object $
      (Key.fromString "type" Aeson..= type_ error_)
        : (fmap (\(k, v) -> Key.fromText k Aeson..= v) . Map.toList) (value error_)
