module Patrol.Type.ProjectId where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype ProjectId
  = ProjectId Text.Text
  deriving (Eq, Show)

fromText :: Exception.MonadThrow m => Text.Text -> m ProjectId
fromText = fmap ProjectId . Text.presence

intoText :: ProjectId -> Text.Text
intoText (ProjectId text) = text
