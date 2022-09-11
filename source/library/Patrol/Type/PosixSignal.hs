module Patrol.Type.PosixSignal where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#posixsignal>
data PosixSignal = PosixSignal
  { code :: Maybe Int,
    codeName :: Maybe Text.Text,
    name :: Maybe Text.Text,
    number :: Maybe Int
  }
  deriving (Eq, Show)

instance Aeson.ToJSON PosixSignal where
  toJSON posixSignal =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "code" Aeson..= code posixSignal,
          Key.fromString "code_name" Aeson..= codeName posixSignal,
          Key.fromString "name" Aeson..= name posixSignal,
          Key.fromString "number" Aeson..= number posixSignal
        ]
