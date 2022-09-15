module Patrol.Type.Geo where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#geo>
data Geo = Geo
  { city :: Text.Text,
    countryCode :: Text.Text,
    region :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Geo where
  toJSON geo =
    Aeson.intoObject
      [ Aeson.pair "city" $ city geo,
        Aeson.pair "country_code" $ countryCode geo,
        Aeson.pair "region" $ region geo
      ]

empty :: Geo
empty =
  Geo
    { city = Text.empty,
      countryCode = Text.empty,
      region = Text.empty
    }
