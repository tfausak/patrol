module Patrol.Type.DebugMeta where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.SystemSdkInfo as SystemSdkInfo

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#debugmeta>
data DebugMeta = DebugMeta
  { images :: [Map.Map Text.Text Aeson.Value],
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
