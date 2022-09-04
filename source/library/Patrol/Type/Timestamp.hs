module Patrol.Type.Timestamp where

import qualified Control.Monad.IO.Class as IO
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

epoch :: Timestamp
epoch = fromUtcTime $ Time.UTCTime (Time.fromGregorian 1970 1 1) 0

now :: IO.MonadIO io => io Timestamp
now = IO.liftIO $ fmap fromUtcTime Time.getCurrentTime
