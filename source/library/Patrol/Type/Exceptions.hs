module Patrol.Type.Exceptions where

import qualified Data.Aeson as Aeson
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.ExceptionValue as ExceptionValue

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#exception>
newtype Exceptions = Exceptions
  { values :: [ExceptionValue.ExceptionValue]
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Exceptions where
  toJSON exceptions =
    Aeson.intoObject
      [ Aeson.pair "values" $ values exceptions
      ]

empty :: Exceptions
empty =
  Exceptions
    { values = []
    }
