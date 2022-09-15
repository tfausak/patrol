module Patrol.Type.ProguardDebugImage where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#proguarddebugimage>
newtype ProguardDebugImage = ProguardDebugImage
  { uuid :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON ProguardDebugImage where
  toJSON proguardDebugImage =
    Aeson.intoObject
      [ Aeson.pair "uuid" $ uuid proguardDebugImage
      ]

empty :: ProguardDebugImage
empty =
  ProguardDebugImage
    { uuid = Text.empty
    }
