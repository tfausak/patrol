module Patrol.Type.MachException where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#machexception>
data MachException = MachException
  { code :: Maybe Int,
    exception :: Maybe Int,
    name :: Text.Text,
    subcode :: Maybe Int
  }
  deriving (Eq, Show)

instance Aeson.ToJSON MachException where
  toJSON machException =
    Aeson.intoObject
      [ Aeson.pair "code" $ code machException,
        Aeson.pair "exception" $ exception machException,
        Aeson.pair "name" $ name machException,
        Aeson.pair "subcode" $ subcode machException
      ]

empty :: MachException
empty =
  MachException
    { code = Nothing,
      exception = Nothing,
      name = Text.empty,
      subcode = Nothing
    }
