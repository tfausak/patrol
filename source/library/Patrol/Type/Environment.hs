module Patrol.Type.Environment where

import qualified Control.Monad.Catch as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype Environment
  = Environment Text.Text
  deriving (Eq, Show)

instance Aeson.ToJSON Environment where
  toJSON = Aeson.toJSON . intoText

fromText :: Exception.MonadThrow m => Text.Text -> m Environment
fromText = fmap Environment . Text.presence

intoText :: Environment -> Text.Text
intoText (Environment text) = text

production :: Environment
production = Environment $ Text.pack "production"
