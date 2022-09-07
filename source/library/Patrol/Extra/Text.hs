module Patrol.Extra.Text where

import qualified Control.Monad.Catch as Catch
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Patrol.Exception.Problem as Problem

presence :: Catch.MonadThrow m => Text.Text -> m Text.Text
presence text =
  if Text.all Char.isSpace text
    then Catch.throwM $ Problem.Problem "empty text"
    else pure text
