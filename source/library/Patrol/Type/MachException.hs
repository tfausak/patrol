module Patrol.Type.MachException where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

data MachException = MachException
  { code :: Int,
    exception :: Int,
    subcode :: Int,
    name :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON MachException where
  toJSON machException =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "code" Aeson..= code machException,
          Key.fromString "exception" Aeson..= exception machException,
          Key.fromString "subcode" Aeson..= subcode machException,
          Key.fromString "name" Aeson..= name machException
        ]
