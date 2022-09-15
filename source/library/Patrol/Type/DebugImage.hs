module Patrol.Type.DebugImage where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.AppleDebugImage as AppleDebugImage
import qualified Patrol.Type.ProguardDebugImage as ProguardDebugImage

data DebugImage
  = Apple AppleDebugImage.AppleDebugImage
  | Proguard ProguardDebugImage.ProguardDebugImage
  | Other (Map.Map Text.Text Aeson.Value)
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#nativedebugimage
  deriving (Eq, Show)

instance Aeson.ToJSON DebugImage where
  toJSON debugImage = case debugImage of
    Apple appleDebugImage -> Aeson.toJSON appleDebugImage
    Proguard proguardDebugImage -> Aeson.toJSON proguardDebugImage
    Other other -> Aeson.toJSON other
