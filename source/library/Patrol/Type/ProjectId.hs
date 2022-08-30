module Patrol.Type.ProjectId where

import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype ProjectId
  = ProjectId Text.Text
  deriving (Eq, Show)

fromText :: Text.Text -> Maybe ProjectId
fromText = fmap ProjectId . Text.presence

toText :: ProjectId -> Text.Text
toText (ProjectId text) = text
