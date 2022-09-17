module Patrol.Type.Context where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.AppContext as AppContext
import qualified Patrol.Type.BrowserContext as BrowserContext
import qualified Patrol.Type.DeviceContext as DeviceContext

data Context
  = App AppContext.AppContext
  | Browser BrowserContext.BrowserContext
  | Device DeviceContext.DeviceContext
  | Other (Map.Map Text.Text Aeson.Value)
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#gpucontext
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#oscontext
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#runtimecontext
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#tracecontext
  deriving (Eq, Show)

instance Aeson.ToJSON Context where
  toJSON context = case context of
    App appContext -> Aeson.toJSON appContext
    Browser browserContext -> Aeson.toJSON browserContext
    Device deviceContext -> Aeson.toJSON deviceContext
    Other other -> Aeson.toJSON other
