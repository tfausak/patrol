module Patrol.Type.Exception where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Mechanism as Mechanism
import qualified Patrol.Type.Stacktrace as Stacktrace

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#typedef-ExceptionValue>
data Exception = Exception
  { mechanism :: Maybe Mechanism.Mechanism,
    module_ :: Maybe Text.Text,
    stacktrace :: Maybe Stacktrace.Stacktrace,
    threadId :: Maybe Text.Text,
    type_ :: Maybe Text.Text,
    value :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Exception where
  toJSON exception =
    Aeson.intoObject
      [ Aeson.pair "mechanism" $ mechanism exception,
        Aeson.pair "module" $ module_ exception,
        Aeson.pair "stacktrace" $ stacktrace exception,
        Aeson.pair "thread_id" $ threadId exception,
        Aeson.pair "type" $ type_ exception,
        Aeson.pair "value" $ value exception
      ]

empty :: Exception
empty =
  Exception
    { mechanism = Nothing,
      module_ = Nothing,
      stacktrace = Nothing,
      threadId = Nothing,
      type_ = Nothing,
      value = Nothing
    }

fromSomeException :: Catch.SomeException -> Exception
fromSomeException (Catch.SomeException e) =
  empty
    { type_ = Just . Text.pack . show $ Typeable.typeOf e,
      value = Just . Text.pack $ Catch.displayException e
    }
