module Patrol.Type.PublicKey where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype PublicKey
  = PublicKey Text.Text
  deriving (Eq, Show)

fromText :: Exception.MonadThrow m => Text.Text -> m PublicKey
fromText = fmap PublicKey . Text.presence

intoText :: PublicKey -> Text.Text
intoText (PublicKey text) = text
