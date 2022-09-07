module Patrol.Type.ModuleVersion where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Text as Text

newtype ModuleVersion
  = ModuleVersion Text.Text
  deriving (Eq, Show)

instance Aeson.ToJSON ModuleVersion where
  toJSON = Aeson.toJSON . intoText

fromText :: Catch.MonadThrow m => Text.Text -> m ModuleVersion
fromText = fmap ModuleVersion . Text.presence

intoText :: ModuleVersion -> Text.Text
intoText (ModuleVersion text) = text
