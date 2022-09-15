module Patrol.Type.NsError where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#nserror>
data NsError = NsError
  { code :: Maybe Int,
    domain :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON NsError where
  toJSON nsError =
    Aeson.intoObject
      [ Aeson.pair "code" $ code nsError,
        Aeson.pair "domain" $ domain nsError
      ]

empty :: NsError
empty =
  NsError
    { code = Nothing,
      domain = Text.empty
    }
