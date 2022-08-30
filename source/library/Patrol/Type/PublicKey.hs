module Patrol.Type.PublicKey where

import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype PublicKey
  = PublicKey Text.Text
  deriving (Eq, Show)

fromText :: Text.Text -> Maybe PublicKey
fromText = fmap PublicKey . Text.presence

toText :: PublicKey -> Text.Text
toText (PublicKey text) = text
