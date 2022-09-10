module Patrol.Type.Meta where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Signal as Signal

newtype Meta = Meta
  { signal :: Maybe Signal.Signal
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Meta where
  toJSON meta =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "signal" Aeson..= signal meta
        ]
