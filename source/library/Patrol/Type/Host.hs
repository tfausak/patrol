module Patrol.Type.Host where

import qualified Control.Monad.Catch as Catch
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype Host
  = Host Text.Text
  deriving (Eq, Show)

fromText :: Catch.MonadThrow m => Text.Text -> m Host
fromText = fmap Host . Text.presence

intoText :: Host -> Text.Text
intoText (Host text) = text
