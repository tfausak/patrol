module Patrol.Type.PosixSignal where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#posixsignal>
data PosixSignal = PosixSignal
  { code :: Maybe Int,
    codeName :: Text.Text,
    name :: Text.Text,
    number :: Maybe Int
  }
  deriving (Eq, Show)

instance Aeson.ToJSON PosixSignal where
  toJSON posixSignal =
    Aeson.intoObject
      [ Aeson.pair "code" $ code posixSignal,
        Aeson.pair "code_name" $ codeName posixSignal,
        Aeson.pair "name" $ name posixSignal,
        Aeson.pair "number" $ number posixSignal
      ]

empty :: PosixSignal
empty =
  PosixSignal
    { code = Nothing,
      codeName = Text.empty,
      name = Text.empty,
      number = Nothing
    }
