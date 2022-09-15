module Patrol.Type.CError where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#cerror>
data CError = CError
  { name :: Text.Text,
    number :: Maybe Int
  }
  deriving (Eq, Show)

instance Aeson.ToJSON CError where
  toJSON cError =
    Aeson.intoObject
      [ Aeson.pair "name" $ name cError,
        Aeson.pair "number" $ number cError
      ]

empty :: CError
empty =
  CError
    { name = Text.empty,
      number = Nothing
    }
