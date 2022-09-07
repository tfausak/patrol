module Patrol.Type.Dist where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype Dist
  = Dist Text.Text
  deriving (Eq, Show)

instance Aeson.ToJSON Dist where
  toJSON = Aeson.toJSON . intoText

fromText :: Catch.MonadThrow m => Text.Text -> m Dist
fromText = fmap Dist . Text.presence

intoText :: Dist -> Text.Text
intoText (Dist text) = text
