module Patrol.Type.SecretKey where

import qualified Control.Monad.Catch as Catch
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype SecretKey
  = SecretKey Text.Text
  deriving (Eq, Show)

fromText :: Catch.MonadThrow m => Text.Text -> m SecretKey
fromText = fmap SecretKey . Text.presence

intoText :: SecretKey -> Text.Text
intoText (SecretKey text) = text
