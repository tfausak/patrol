module Patrol.Type.DebugMeta where

import qualified Data.Aeson as Aeson
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.DebugImage as DebugImage
import qualified Patrol.Type.SystemSdkInfo as SystemSdkInfo

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#debugmeta>
data DebugMeta = DebugMeta
  { images :: [DebugImage.DebugImage],
    sdkInfo :: Maybe SystemSdkInfo.SystemSdkInfo
  }
  deriving (Eq, Show)

instance Aeson.ToJSON DebugMeta where
  toJSON debugMeta =
    Aeson.intoObject
      [ Aeson.pair "images" $ images debugMeta,
        Aeson.pair "sdk_info" $ sdkInfo debugMeta
      ]

empty :: DebugMeta
empty =
  DebugMeta
    { images = [],
      sdkInfo = Nothing
    }
