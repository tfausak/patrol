module Patrol.Type.GpuContext where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#gpucontext>
data GpuContext = GpuContext
  { apiType :: Text.Text,
    graphicsShaderLevel :: Text.Text,
    id :: Aeson.Value,
    maxTextureSize :: Maybe Int,
    memorySize :: Maybe Double,
    multiThreadedRendering :: Maybe Bool,
    name :: Text.Text,
    npotSupport :: Text.Text,
    supportsComputeShaders :: Maybe Bool,
    supportsGeometryShaders :: Maybe Bool,
    supportsRayTracing :: Maybe Bool,
    vendorId :: Text.Text,
    vendorName :: Text.Text,
    version :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON GpuContext where
  toJSON gpuContext =
    Aeson.intoObject
      [ Aeson.pair "api_type" $ apiType gpuContext,
        Aeson.pair "graphics_shader_level" $ graphicsShaderLevel gpuContext,
        Aeson.pair "id" $ Patrol.Type.GpuContext.id gpuContext,
        Aeson.pair "max_texture_size" $ maxTextureSize gpuContext,
        Aeson.pair "memory_size" $ memorySize gpuContext,
        Aeson.pair "multi_threaded_rendering" $ multiThreadedRendering gpuContext,
        Aeson.pair "name" $ name gpuContext,
        Aeson.pair "npot_support" $ npotSupport gpuContext,
        Aeson.pair "supports_compute_shaders" $ supportsComputeShaders gpuContext,
        Aeson.pair "supports_geometry_shaders" $ supportsGeometryShaders gpuContext,
        Aeson.pair "supports_ray_tracing" $ supportsRayTracing gpuContext,
        Aeson.pair "vendor_id" $ vendorId gpuContext,
        Aeson.pair "vendor_name" $ vendorName gpuContext,
        Aeson.pair "version" $ version gpuContext
      ]

empty :: GpuContext
empty =
  GpuContext
    { apiType = Text.empty,
      graphicsShaderLevel = Text.empty,
      Patrol.Type.GpuContext.id = Aeson.Null,
      maxTextureSize = Nothing,
      memorySize = Nothing,
      multiThreadedRendering = Nothing,
      name = Text.empty,
      npotSupport = Text.empty,
      supportsComputeShaders = Nothing,
      supportsGeometryShaders = Nothing,
      supportsRayTracing = Nothing,
      vendorId = Text.empty,
      vendorName = Text.empty,
      version = Text.empty
    }
