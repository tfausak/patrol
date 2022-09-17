module Patrol.Type.BrowserContext where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#browsercontext>
data BrowserContext = BrowserContext
  { name :: Text.Text,
    version :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON BrowserContext where
  toJSON browserContext =
    Aeson.intoObject
      [ Aeson.pair "name" $ name browserContext,
        Aeson.pair "version" $ version browserContext
      ]

empty :: BrowserContext
empty =
  BrowserContext
    { name = Text.empty,
      version = Text.empty
    }
