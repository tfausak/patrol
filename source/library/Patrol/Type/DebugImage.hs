module Patrol.Type.DebugImage where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.AppleDebugImage as AppleDebugImage
import qualified Patrol.Type.NativeDebugImage as NativeDebugImage
import qualified Patrol.Type.ProguardDebugImage as ProguardDebugImage

data DebugImage
  = Apple AppleDebugImage.AppleDebugImage
  | Native NativeDebugImage.NativeDebugImage
  | Proguard ProguardDebugImage.ProguardDebugImage
  | Other (Map.Map Text.Text Aeson.Value)
  deriving (Eq, Show)

instance Aeson.ToJSON DebugImage where
  toJSON debugImage = case debugImage of
    Apple appleDebugImage -> Aeson.toJSON appleDebugImage
    Native nativeDebugImage -> Aeson.toJSON nativeDebugImage
    Proguard proguardDebugImage -> Aeson.toJSON proguardDebugImage
    Other other -> Aeson.toJSON other
