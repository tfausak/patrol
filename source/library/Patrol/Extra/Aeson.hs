module Patrol.Extra.Aeson where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text

intoObject :: [Aeson.Pair] -> Aeson.Value
intoObject = Aeson.object . filter (not . isEmpty . snd)

isEmpty :: Aeson.Value -> Bool
isEmpty value = case value of
  Aeson.Array array -> null array
  Aeson.Null -> True
  Aeson.Object object -> null object
  Aeson.String string -> Text.null string
  _ -> False

pair :: (Aeson.ToJSON a) => String -> a -> Aeson.Pair
pair = (Aeson..=) . Key.fromString
