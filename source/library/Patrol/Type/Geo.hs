module Patrol.Type.Geo where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#geo>
data Geo = Geo
  { city :: Maybe Text.Text,
    countryCode :: Maybe Text.Text,
    region :: Maybe Text.Text
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
    { city = Nothing,
      countryCode = Nothing,
      region = Nothing
    }
