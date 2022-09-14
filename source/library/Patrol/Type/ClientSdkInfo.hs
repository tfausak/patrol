module Patrol.Type.ClientSdkInfo where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.ClientSdkPackage as ClientSdkPackage

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#clientsdkinfo>
data ClientSdkInfo = ClientSdkInfo
  { integrations :: [Text.Text],
    name :: Maybe Text.Text,
    packages :: [ClientSdkPackage.ClientSdkPackage],
    version :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON ClientSdkInfo where
  toJSON clientSdkInfo =
    Aeson.intoObject
      [ Aeson.pair "integrations" $ integrations clientSdkInfo,
        Aeson.pair "name" $ name clientSdkInfo,
        Aeson.pair "packages" $ packages clientSdkInfo,
        Aeson.pair "version" $ version clientSdkInfo
      ]

empty :: ClientSdkInfo
empty =
  ClientSdkInfo
    { integrations = [],
      name = Nothing,
      packages = [],
      version = Nothing
    }
