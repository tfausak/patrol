module Patrol.Type.NativeDebugImage where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#nativedebugimage>
data NativeDebugImage = NativeDebugImage
  { arch :: Text.Text,
    codeFile :: Text.Text,
    codeId :: Text.Text,
    debugFile :: Text.Text,
    debugId :: Text.Text,
    imageAddr :: Text.Text,
    imageSize :: Maybe Int,
    imageVmaddr :: Text.Text,
    type_ :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON NativeDebugImage where
  toJSON nativeDebugImage =
    Aeson.intoObject
      [ Aeson.pair "arch" $ arch nativeDebugImage,
        Aeson.pair "code_file" $ codeFile nativeDebugImage,
        Aeson.pair "code_id" $ codeId nativeDebugImage,
        Aeson.pair "debug_file" $ debugFile nativeDebugImage,
        Aeson.pair "debug_id" $ debugId nativeDebugImage,
        Aeson.pair "image_addr" $ imageAddr nativeDebugImage,
        Aeson.pair "image_size" $ imageSize nativeDebugImage,
        Aeson.pair "image_vmaddr" $ imageVmaddr nativeDebugImage,
        Aeson.pair "type" $ type_ nativeDebugImage
      ]

empty :: NativeDebugImage
empty =
  NativeDebugImage
    { arch = Text.empty,
      codeFile = Text.empty,
      codeId = Text.empty,
      debugFile = Text.empty,
      debugId = Text.empty,
      imageAddr = Text.empty,
      imageSize = Nothing,
      imageVmaddr = Text.empty,
      type_ = Text.empty
    }
