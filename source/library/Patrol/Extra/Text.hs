module Patrol.Extra.Text where

import qualified Data.Text as Text

presence :: Text.Text -> Maybe Text.Text
presence text = if Text.null text then Nothing else Just text
