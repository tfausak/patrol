module Patrol.Extra.Text where

import qualified Control.Monad.Catch as Exception
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Patrol.Exception.Problem as Problem

presence :: Exception.MonadThrow m => Text.Text -> m Text.Text
presence text =
  if Text.all Char.isSpace text
    then Exception.throwM $ Problem.Problem "empty text"
    else pure text
