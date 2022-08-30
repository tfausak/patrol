module Patrol.Type.Protocol where

import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype Protocol
  = Protocol Text.Text
  deriving (Eq, Show)

fromText :: Text.Text -> Maybe Protocol
fromText = fmap Protocol . Text.presence

toText :: Protocol -> Text.Text
toText (Protocol text) = text
