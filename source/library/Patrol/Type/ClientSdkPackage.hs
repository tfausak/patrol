module Patrol.Type.ClientSdkPackage where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#clientsdkpackage>
data ClientSdkPackage = ClientSdkPackage
  { name :: Maybe Text.Text,
    version :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON ClientSdkPackage where
  toJSON clientSdkPackage =
    Aeson.intoObject
      [ Aeson.pair "name" $ name clientSdkPackage,
        Aeson.pair "version" $ version clientSdkPackage
      ]

empty :: ClientSdkPackage
empty =
  ClientSdkPackage
    { name = Nothing,
      version = Nothing
    }
