module Patrol.Type.Host where

import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype Host
  = Host Text.Text
  deriving (Eq, Show)

fromText :: Text.Text -> Maybe Host
fromText = fmap Host . Text.presence

toText :: Host -> Text.Text
toText (Host text) = text
