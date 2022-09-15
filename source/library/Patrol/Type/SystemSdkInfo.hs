module Patrol.Type.SystemSdkInfo where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#systemsdkinfo>
data SystemSdkInfo = SystemSdkInfo
  { sdkName :: Text.Text,
    versionMajor :: Maybe Int,
    versionMinor :: Maybe Int,
    versionPatchlevel :: Maybe Int
  }
  deriving (Eq, Show)

instance Aeson.ToJSON SystemSdkInfo where
  toJSON cError =
    Aeson.intoObject
      [ Aeson.pair "sdk_name" $ sdkName cError,
        Aeson.pair "version_major" $ versionMajor cError,
        Aeson.pair "version_minor" $ versionMinor cError,
        Aeson.pair "version_patchlevel" $ versionPatchlevel cError
      ]

empty :: SystemSdkInfo
empty =
  SystemSdkInfo
    { sdkName = Text.empty,
      versionMajor = Nothing,
      versionMinor = Nothing,
      versionPatchlevel = Nothing
    }
