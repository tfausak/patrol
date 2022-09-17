module Patrol.Type.OsContext where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#oscontext>
data OsContext = OsContext
  { build :: Text.Text,
    kernelVersion :: Text.Text,
    name :: Text.Text,
    rawDescription :: Text.Text,
    rooted :: Maybe Bool,
    version :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON OsContext where
  toJSON osContext =
    Aeson.intoObject
      [ Aeson.pair "build" $ build osContext,
        Aeson.pair "kernel_version" $ kernelVersion osContext,
        Aeson.pair "name" $ name osContext,
        Aeson.pair "raw_description" $ rawDescription osContext,
        Aeson.pair "rooted" $ rooted osContext,
        Aeson.pair "version" $ version osContext
      ]

empty :: OsContext
empty =
  OsContext
    { build = Text.empty,
      kernelVersion = Text.empty,
      name = Text.empty,
      rawDescription = Text.empty,
      rooted = Nothing,
      version = Text.empty
    }
