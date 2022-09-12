module Patrol.Type.Exception where

import qualified Data.Aeson as Aeson
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.ExceptionValue as ExceptionValue

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#exception>
newtype Exception = Exception
  { values :: [ExceptionValue.ExceptionValue]
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Exception where
  toJSON exception =
    Aeson.intoObject
      [ Aeson.pair "values" $ values exception
      ]

empty :: Exception
empty =
  Exception
    { values = []
    }
