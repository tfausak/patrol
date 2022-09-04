module Patrol.Type.Transaction where

import qualified Control.Monad.Catch as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype Transaction
  = Transaction Text.Text
  deriving (Eq, Show)

instance Aeson.ToJSON Transaction where
  toJSON = Aeson.toJSON . intoText

fromText :: Exception.MonadThrow m => Text.Text -> m Transaction
fromText = fmap Transaction . Text.presence

intoText :: Transaction -> Text.Text
intoText (Transaction text) = text
