module Patrol.Type.ModuleName where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Functor.Contravariant as Contravariant
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype ModuleName
  = ModuleName Text.Text
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON ModuleName where
  toJSON = Aeson.toJSON . intoText

instance Aeson.ToJSONKey ModuleName where
  toJSONKey = Contravariant.contramap intoText Aeson.toJSONKey

fromText :: Catch.MonadThrow m => Text.Text -> m ModuleName
fromText = fmap ModuleName . Text.presence

intoText :: ModuleName -> Text.Text
intoText (ModuleName text) = text
