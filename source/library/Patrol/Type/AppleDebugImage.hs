module Patrol.Type.AppleDebugImage where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#appledebugimage>
data AppleDebugImage = AppleDebugImage
  { arch :: Text.Text,
    cpuSubtype :: Maybe Int,
    cpuType :: Maybe Int,
    imageAddr :: Text.Text,
    imageSize :: Maybe Int,
    imageVmaddr :: Text.Text,
    name :: Text.Text,
    uuid :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON AppleDebugImage where
  toJSON appleDebugImage =
    Aeson.intoObject
      [ Aeson.pair "arch" $ arch appleDebugImage,
        Aeson.pair "cpu_subtype" $ cpuSubtype appleDebugImage,
        Aeson.pair "cpu_type" $ cpuType appleDebugImage,
        Aeson.pair "image_addr" $ imageAddr appleDebugImage,
        Aeson.pair "image_size" $ imageSize appleDebugImage,
        Aeson.pair "image_vmaddr" $ imageVmaddr appleDebugImage,
        Aeson.pair "name" $ name appleDebugImage,
        Aeson.pair "uuid" $ uuid appleDebugImage
      ]

empty :: AppleDebugImage
empty =
  AppleDebugImage
    { arch = Text.empty,
      cpuSubtype = Nothing,
      cpuType = Nothing,
      imageAddr = Text.empty,
      imageSize = Nothing,
      imageVmaddr = Text.empty,
      name = Text.empty,
      uuid = Text.empty
    }
