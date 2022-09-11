module Patrol.Type.ExceptionValue where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Mechanism as Mechanism
import qualified Patrol.Type.Stacktrace as Stacktrace

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#typedef-ExceptionValue>
data ExceptionValue = ExceptionValue
  { mechanism :: Maybe Mechanism.Mechanism,
    module_ :: Maybe Text.Text,
    stacktrace :: Maybe Stacktrace.Stacktrace,
    threadId :: Maybe Text.Text,
    type_ :: Maybe Text.Text,
    value :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON ExceptionValue where
  toJSON exceptionValue =
    Aeson.intoObject
      [ Aeson.pair "mechanism" $ mechanism exceptionValue,
        Aeson.pair "module" $ module_ exceptionValue,
        Aeson.pair "stacktrace" $ stacktrace exceptionValue,
        Aeson.pair "thread_id" $ threadId exceptionValue,
        Aeson.pair "type" $ type_ exceptionValue,
        Aeson.pair "value" $ value exceptionValue
      ]

fromSomeException :: Catch.SomeException -> ExceptionValue
fromSomeException (Catch.SomeException e) =
  ExceptionValue
    { mechanism = Nothing,
      module_ = Nothing,
      stacktrace = Nothing,
      threadId = Nothing,
      type_ = Just . Text.pack . show $ Typeable.typeOf e,
      value = Just . Text.pack $ Catch.displayException e
    }
