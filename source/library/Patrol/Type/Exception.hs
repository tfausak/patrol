module Patrol.Type.Exception where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified GHC.Stack as Stack
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Mechanism as Mechanism
import qualified Patrol.Type.Stacktrace as Stacktrace

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#valueclass>
data Exception = Exception
  { mechanism :: Maybe Mechanism.Mechanism,
    module_ :: Text.Text,
    stacktrace :: Maybe Stacktrace.Stacktrace,
    threadId :: Text.Text,
    type_ :: Text.Text,
    value :: Text.Text
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
      module_ = Text.empty,
      stacktrace = Nothing,
      threadId = Text.empty,
      type_ = Text.empty,
      value = Text.empty
    }

fromException ::
  (Catch.Exception e) =>
  (Catch.SomeException -> Maybe Stack.CallStack) ->
  e ->
  Exception
fromException getCallStack e =
  empty
    { stacktrace = fmap Stacktrace.fromCallStack . getCallStack $ Catch.toException e,
      type_ = Text.pack . show $ Typeable.typeOf e,
      value = Text.pack $ Catch.displayException e
    }
