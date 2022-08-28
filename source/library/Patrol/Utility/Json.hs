module Patrol.Utility.Json
  ( pair,
    required,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.String as String

pair :: (Aeson.ToJSON value, Aeson.KeyValue pair) => String -> value -> pair
pair key value = String.fromString key Aeson..= value

required :: Aeson.FromJSON value => Aeson.Object -> String -> Aeson.Parser value
required object key = object Aeson..: String.fromString key
