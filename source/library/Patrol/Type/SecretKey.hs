module Patrol.Type.SecretKey where

import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype SecretKey
  = SecretKey Text.Text
  deriving (Eq, Show)

fromText :: Text.Text -> Maybe SecretKey
fromText = fmap SecretKey . Text.presence

toText :: SecretKey -> Text.Text
toText (SecretKey text) = text
