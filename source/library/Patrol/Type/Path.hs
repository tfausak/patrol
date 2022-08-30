module Patrol.Type.Path where

import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype Path
  = Path Text.Text
  deriving (Eq, Show)

fromText :: Text.Text -> Maybe Path
fromText = fmap Path . Text.presence

toText :: Path -> Text.Text
toText (Path text) = text
