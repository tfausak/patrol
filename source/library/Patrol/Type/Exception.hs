module Patrol.Type.Exception where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Patrol.Extra.Aeson as Aeson

data Exception = Exception
  { module_ :: Maybe Text.Text,
    type_ :: Text.Text,
    value :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Exception where
  toJSON exception =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "module" Aeson..= module_ exception,
          Key.fromString "type" Aeson..= type_ exception,
          Key.fromString "value" Aeson..= value exception
        ]

fromSomeException :: Catch.SomeException -> Exception
fromSomeException (Catch.SomeException e) =
  Exception
    { module_ = Nothing,
      type_ = Text.pack . show $ Typeable.typeOf e,
      value = Just . Text.pack $ Catch.displayException e
    }
