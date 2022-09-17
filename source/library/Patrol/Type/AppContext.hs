module Patrol.Type.AppContext where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#appcontext>
data AppContext = AppContext
  { appBuild :: Text.Text,
    appIdentifier :: Text.Text,
    appMemory :: Maybe Int,
    appName :: Text.Text,
    appStartTime :: Maybe Time.UTCTime,
    appVersion :: Text.Text,
    buildType :: Text.Text,
    deviceAppHash :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON AppContext where
  toJSON appContext =
    Aeson.intoObject
      [ Aeson.pair "app_build" $ appBuild appContext,
        Aeson.pair "app_identifier" $ appIdentifier appContext,
        Aeson.pair "app_memory" $ appMemory appContext,
        Aeson.pair "app_name" $ appName appContext,
        Aeson.pair "app_start_time" $ appStartTime appContext,
        Aeson.pair "app_version" $ appVersion appContext,
        Aeson.pair "build_type" $ buildType appContext,
        Aeson.pair "device_app_hash" $ deviceAppHash appContext
      ]

empty :: AppContext
empty =
  AppContext
    { appBuild = Text.empty,
      appIdentifier = Text.empty,
      appMemory = Nothing,
      appName = Text.empty,
      appStartTime = Nothing,
      appVersion = Text.empty,
      buildType = Text.empty,
      deviceAppHash = Text.empty
    }
