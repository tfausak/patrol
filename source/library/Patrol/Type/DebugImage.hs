module Patrol.Type.DebugImage where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text

newtype DebugImage
  = Other (Map.Map Text.Text Aeson.Value)
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#appledebugimage
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#nativedebugimage
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#proguarddebugimage
  deriving (Eq, Show)

instance Aeson.ToJSON DebugImage where
  toJSON debugImage = case debugImage of
    Other other -> Aeson.toJSON other
