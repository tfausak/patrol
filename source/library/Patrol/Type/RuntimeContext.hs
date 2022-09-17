module Patrol.Type.RuntimeContext where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#runtimecontext>
data RuntimeContext = RuntimeContext
  { build :: Text.Text,
    name :: Text.Text,
    rawDescription :: Text.Text,
    version :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON RuntimeContext where
  toJSON runtimeContext =
    Aeson.intoObject
      [ Aeson.pair "build" $ build runtimeContext,
        Aeson.pair "name" $ name runtimeContext,
        Aeson.pair "raw_description" $ rawDescription runtimeContext,
        Aeson.pair "version" $ version runtimeContext
      ]

empty :: RuntimeContext
empty =
  RuntimeContext
    { build = Text.empty,
      name = Text.empty,
      rawDescription = Text.empty,
      version = Text.empty
    }
