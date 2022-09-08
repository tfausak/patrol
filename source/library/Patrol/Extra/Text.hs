module Patrol.Extra.Text where

import qualified Control.Monad.Catch as Catch
import qualified Data.Text as Text
import qualified Patrol.Type.NonEmptyText as NonEmptyText

presence :: Catch.MonadThrow m => Text.Text -> m Text.Text
presence = fmap NonEmptyText.intoText . NonEmptyText.fromText
