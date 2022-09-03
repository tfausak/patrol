module Patrol.Type.Timestamp where

import qualified Data.Aeson as Aeson
import qualified Data.Time as Time

newtype Timestamp
  = Timestamp Time.UTCTime
  deriving (Eq, Show)

instance Aeson.ToJSON Timestamp where
  toJSON = Aeson.toJSON . intoUtcTime

fromUtcTime :: Time.UTCTime -> Timestamp
fromUtcTime = Timestamp

intoUtcTime :: Timestamp -> Time.UTCTime
intoUtcTime (Timestamp utcTime) = utcTime
