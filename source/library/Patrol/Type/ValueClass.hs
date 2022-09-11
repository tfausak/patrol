module Patrol.Type.ValueClass where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Mechanism as Mechanism
import qualified Patrol.Type.Stacktrace as Stacktrace

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#typedef-ValueClass>
data ValueClass = ValueClass
  { mechanism :: Maybe Mechanism.Mechanism,
    module_ :: Maybe Text.Text,
    stacktrace :: Maybe Stacktrace.Stacktrace,
    threadId :: Maybe Text.Text,
    type_ :: Maybe Text.Text,
    value :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON ValueClass where
  toJSON valueClass =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "mechanism" Aeson..= mechanism valueClass,
          Key.fromString "module" Aeson..= module_ valueClass,
          Key.fromString "stacktrace" Aeson..= stacktrace valueClass,
          Key.fromString "thread_id" Aeson..= threadId valueClass,
          Key.fromString "type" Aeson..= type_ valueClass,
          Key.fromString "value" Aeson..= value valueClass
        ]

fromSomeException :: Catch.SomeException -> ValueClass
fromSomeException (Catch.SomeException e) =
  ValueClass
    { mechanism = Nothing,
      module_ = Nothing,
      stacktrace = Nothing,
      threadId = Nothing,
      type_ = Just . Text.pack . show $ Typeable.typeOf e,
      value = Just . Text.pack $ Catch.displayException e
    }
