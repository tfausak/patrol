module Patrol.Type.Path where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype Path
  = Path Text.Text
  deriving (Eq, Show)

fromText :: Exception.MonadThrow m => Text.Text -> m Path
fromText = fmap Path . Text.presence

intoText :: Path -> Text.Text
intoText (Path text) = text
