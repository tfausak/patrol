module Patrol.Type.Exception where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Mechanism as Mechanism
import qualified Patrol.Type.StackTrace as StackTrace

data Exception = Exception
  { mechanism :: Maybe Mechanism.Mechanism,
    module_ :: Maybe Text.Text,
    stacktrace :: Maybe StackTrace.StackTrace,
    threadId :: Maybe Text.Text,
    type_ :: Text.Text,
    value :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Exception where
  toJSON exception =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "mechanism" Aeson..= mechanism exception,
          Key.fromString "module" Aeson..= module_ exception,
          Key.fromString "stacktrace" Aeson..= stacktrace exception,
          Key.fromString "thread_id" Aeson..= threadId exception,
          Key.fromString "type" Aeson..= type_ exception,
          Key.fromString "value" Aeson..= value exception
        ]

fromSomeException :: Catch.SomeException -> Exception
fromSomeException (Catch.SomeException e) =
  Exception
    { mechanism = Nothing,
      module_ = Nothing,
      stacktrace = Nothing,
      threadId = Nothing,
      type_ = Text.pack . show $ Typeable.typeOf e,
      value = Just . Text.pack $ Catch.displayException e
    }
