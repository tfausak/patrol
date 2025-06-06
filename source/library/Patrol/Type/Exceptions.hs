module Patrol.Type.Exceptions where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified GHC.Stack as Stack
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Exception as Exception

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#exception>
newtype Exceptions = Exceptions
  { values :: [Exception.Exception]
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

fromException ::
  (Catch.Exception e) =>
  (Catch.SomeException -> Maybe Stack.CallStack) ->
  e ->
  Exceptions
fromException getCallStack e =
  Exceptions
    { values = [Exception.fromException getCallStack e]
    }
