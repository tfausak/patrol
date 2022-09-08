module Patrol.Type.NonEmptyText where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Patrol.Exception.Problem as Problem

newtype NonEmptyText
  = NonEmptyText Text.Text
  deriving (Eq, Show)

instance Aeson.ToJSON NonEmptyText where
  toJSON = Aeson.toJSON . intoText

fromText :: Catch.MonadThrow m => Text.Text -> m NonEmptyText
fromText text =
  if Text.all Char.isSpace text
    then Catch.throwM $ Problem.Problem "empty text"
    else pure $ NonEmptyText text

intoText :: NonEmptyText -> Text.Text
intoText (NonEmptyText text) = text
