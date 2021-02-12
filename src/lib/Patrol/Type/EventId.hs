module Patrol.Type.EventId
  ( EventId
  , fromUuid
  , toUuid
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.UUID as Uuid

newtype EventId
  = EventId Uuid.UUID
  deriving (Eq, Show)

instance Aeson.ToJSON EventId where
  toJSON = Aeson.toJSON . Text.filter (/= '-') . Uuid.toText . toUuid

fromUuid :: Uuid.UUID -> EventId
fromUuid = EventId

toUuid :: EventId -> Uuid.UUID
toUuid (EventId x) = x
