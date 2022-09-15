module Patrol.Type.Context where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text

newtype Context
  = Other (Map.Map Text.Text Aeson.Value)
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#appcontext
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#browsercontext
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#devicecontext
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#gpucontext
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#oscontext
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#runtimecontext
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#tracecontext
  deriving (Eq, Show)

instance Aeson.ToJSON Context where
  toJSON context = case context of
    Other other -> Aeson.toJSON other
