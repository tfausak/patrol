module Patrol.Type.Protocol where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype Protocol
  = Protocol Text.Text
  deriving (Eq, Show)

fromText :: Exception.MonadThrow m => Text.Text -> m Protocol
fromText = fmap Protocol . Text.presence

intoText :: Protocol -> Text.Text
intoText (Protocol text) = text
