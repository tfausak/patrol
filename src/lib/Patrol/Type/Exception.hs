module Patrol.Type.Exception
  ( Exception(..)
  , fromSomeException
  ) where

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Patrol.Type.StackTrace as StackTrace
import qualified Patrol.Utility.Json as Json

data Exception = Exception
  { module_ :: Maybe Text.Text
  , stackTrace :: Maybe StackTrace.StackTrace
  , type_ :: Text.Text
  , value :: Text.Text
  } deriving (Eq, Show)

instance Aeson.ToJSON Exception where
  toJSON exception = Aeson.object $ Maybe.catMaybes
    [ Json.pair "module" <$> module_ exception
    , Json.pair "stacktrace" <$> stackTrace exception
    , Just . Json.pair "type" $ type_ exception
    , Just . Json.pair "value" $ value exception
    ]

fromSomeException :: Exception.SomeException -> Exception
fromSomeException (Exception.SomeException x) =
  let tyCon = Typeable.typeRepTyCon $ Typeable.typeOf x
  in Exception
  { module_ = Just . Text.pack $ Typeable.tyConPackage tyCon <> ":" <> Typeable.tyConModule tyCon
  , stackTrace = Nothing
  , type_ = Text.pack $ Typeable.tyConName tyCon
  , value = Text.pack $ Exception.displayException x
  }
