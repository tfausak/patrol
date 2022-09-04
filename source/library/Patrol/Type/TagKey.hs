module Patrol.Type.TagKey where

import qualified Control.Monad.Catch as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Functor.Contravariant as Contravariant
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype TagKey
  = TagKey Text.Text
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON TagKey where
  toJSON = Aeson.toJSON . intoText

instance Aeson.ToJSONKey TagKey where
  toJSONKey = Contravariant.contramap intoText Aeson.toJSONKey

fromText :: Exception.MonadThrow m => Text.Text -> m TagKey
fromText = fmap TagKey . Text.presence

intoText :: TagKey -> Text.Text
intoText (TagKey text) = text
