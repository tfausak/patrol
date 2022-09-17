module Patrol.Type.Context where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.AppContext as AppContext
import qualified Patrol.Type.BrowserContext as BrowserContext
import qualified Patrol.Type.DeviceContext as DeviceContext
import qualified Patrol.Type.GpuContext as GpuContext
import qualified Patrol.Type.OsContext as OsContext
import qualified Patrol.Type.RuntimeContext as RuntimeContext

data Context
  = App AppContext.AppContext
  | Browser BrowserContext.BrowserContext
  | Device DeviceContext.DeviceContext
  | Gpu GpuContext.GpuContext
  | Os OsContext.OsContext
  | Runtime RuntimeContext.RuntimeContext
  | Other (Map.Map Text.Text Aeson.Value)
  -- TODO: https://develop.sentry.dev/sdk/event-payloads/types/#tracecontext
  deriving (Eq, Show)

instance Aeson.ToJSON Context where
  toJSON context = case context of
    App appContext -> Aeson.toJSON appContext
    Browser browserContext -> Aeson.toJSON browserContext
    Device deviceContext -> Aeson.toJSON deviceContext
    Gpu gpuContext -> Aeson.toJSON gpuContext
    Os osContext -> Aeson.toJSON osContext
    Runtime runtimeContext -> Aeson.toJSON runtimeContext
    Other other -> Aeson.toJSON other
