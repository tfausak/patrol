module Patrol.Type.Release where

import qualified Control.Monad.Catch as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype Release
  = Release Text.Text
  deriving (Eq, Show)

instance Aeson.ToJSON Release where
  toJSON = Aeson.toJSON . intoText

fromText :: Exception.MonadThrow m => Text.Text -> m Release
fromText = fmap Release . Text.presence

intoText :: Release -> Text.Text
intoText (Release text) = text
