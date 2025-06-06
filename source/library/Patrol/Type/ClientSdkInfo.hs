module Patrol.Type.ClientSdkInfo where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.ClientSdkPackage as ClientSdkPackage
import qualified Patrol.Version as Version

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#clientsdkinfo>
data ClientSdkInfo = ClientSdkInfo
  { integrations :: [Text.Text],
    name :: Text.Text,
    packages :: [ClientSdkPackage.ClientSdkPackage],
    version :: Text.Text
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
      name = Text.empty,
      packages = [],
      version = Text.empty
    }

patrol :: ClientSdkInfo
patrol =
  empty
    { name = Text.pack "patrol",
      version = Version.text
    }
