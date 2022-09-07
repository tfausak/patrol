module Patrol.Type.TagValue where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype TagValue
  = TagValue Text.Text
  deriving (Eq, Show)

instance Aeson.ToJSON TagValue where
  toJSON = Aeson.toJSON . intoText

fromText :: Catch.MonadThrow m => Text.Text -> m TagValue
fromText = fmap TagValue . Text.presence

intoText :: TagValue -> Text.Text
intoText (TagValue text) = text
