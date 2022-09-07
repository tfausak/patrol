module Patrol.Type.ServerName where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype ServerName
  = ServerName Text.Text
  deriving (Eq, Show)

instance Aeson.ToJSON ServerName where
  toJSON = Aeson.toJSON . intoText

fromText :: Catch.MonadThrow m => Text.Text -> m ServerName
fromText = fmap ServerName . Text.presence

intoText :: ServerName -> Text.Text
intoText (ServerName text) = text
