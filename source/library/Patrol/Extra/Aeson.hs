module Patrol.Extra.Aeson where

import qualified Data.Aeson as Aeson

isEmpty :: Aeson.Value -> Bool
isEmpty value = case value of
  Aeson.Array array -> null array
  Aeson.Null -> True
  Aeson.Object object -> null object
  _ -> False
