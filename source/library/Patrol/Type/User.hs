module Patrol.Type.User where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Geo as Geo

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#user>
data User = User
  { data_ :: Map.Map Text.Text Aeson.Value,
    email :: Text.Text,
    geo :: Maybe Geo.Geo,
    id :: Text.Text,
    ipAddress :: Text.Text,
    name :: Text.Text,
    segment :: Text.Text,
    username :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON User where
  toJSON user =
    Aeson.intoObject
      [ Aeson.pair "data" $ data_ user,
        Aeson.pair "email" $ email user,
        Aeson.pair "geo" $ geo user,
        Aeson.pair "id" $ Patrol.Type.User.id user,
        Aeson.pair "ip_address" $ ipAddress user,
        Aeson.pair "name" $ name user,
        Aeson.pair "segment" $ segment user,
        Aeson.pair "username" $ username user
      ]

empty :: User
empty =
  User
    { data_ = Map.empty,
      email = Text.empty,
      geo = Nothing,
      Patrol.Type.User.id = Text.empty,
      ipAddress = Text.empty,
      name = Text.empty,
      segment = Text.empty,
      username = Text.empty
    }
