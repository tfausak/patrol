module Patrol.Type.Logger where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype Logger
  = Logger Text.Text
  deriving (Eq, Show)

instance Aeson.ToJSON Logger where
  toJSON = Aeson.toJSON . intoText

fromText :: Catch.MonadThrow m => Text.Text -> m Logger
fromText = fmap Logger . Text.presence

intoText :: Logger -> Text.Text
intoText (Logger text) = text
