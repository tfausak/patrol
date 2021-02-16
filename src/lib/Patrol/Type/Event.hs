module Patrol.Type.Event
  ( Event(..)
  , new
  ) where

import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.UUID.V4 as Uuid
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Exception as Exception
import qualified Patrol.Type.Level as Level
import qualified Patrol.Type.Platform as Platform
import qualified Patrol.Type.Request as Request
import qualified Patrol.Type.Timestamp as Timestamp
import qualified Patrol.Utility.Json as Json

-- | <https://develop.sentry.dev/sdk/event-payloads/>
data Event = Event
  { dist :: Maybe Text.Text
  , environment :: Maybe Text.Text
  , eventId :: EventId.EventId
  , exception :: Maybe [Exception.Exception]
  , extra :: Maybe Aeson.Object
  , fingerprint :: Maybe [Text.Text]
  , level :: Maybe Level.Level
  , logger :: Maybe Text.Text
  , modules :: Maybe (Map.Map Text.Text Text.Text)
  , platform :: Platform.Platform
  , release :: Maybe Text.Text
  , request :: Maybe Request.Request
  , serverName :: Maybe Text.Text
  , tags :: Maybe (Map.Map Text.Text Text.Text)
  , timestamp :: Timestamp.Timestamp
  , transaction :: Maybe Text.Text
  } deriving (Eq, Show)

instance Aeson.ToJSON Event where
  toJSON event = Aeson.object $ Maybe.catMaybes
    [ Json.pair "dist" <$> dist event
    , Json.pair "environment" <$> environment event
    , Just . Json.pair "event_id" $ eventId event
    , Json.pair "extra" <$> extra event
    , Json.pair "exception" . Aeson.object . pure . Json.pair "values" <$> exception event
    , Json.pair "fingerprint" <$> fingerprint event
    , Json.pair "level" <$> level event
    , Json.pair "logger" <$> logger event
    , Json.pair "modules" <$> modules event
    , Just . Json.pair "platform" $ platform event
    , Json.pair "release" <$> release event
    , Json.pair "request" <$> request event
    , Json.pair "server_name" <$> serverName event
    , Json.pair "tags" <$> tags event
    , Just . Json.pair "timestamp" $ timestamp event
    , Json.pair "transaction" <$> transaction event
    ]

new :: IO.MonadIO io => io Event
new = IO.liftIO $ do
  eventId <- EventId.fromUuid <$> Uuid.nextRandom
  timestamp <- Timestamp.fromUtcTime <$> Time.getCurrentTime
  pure Event
    { dist = Nothing
    , environment = Nothing
    , eventId
    , exception = Nothing
    , extra = Nothing
    , fingerprint = Nothing
    , level = Nothing
    , logger = Nothing
    , modules = Nothing
    , platform = Platform.Haskell
    , release = Nothing
    , request = Nothing
    , serverName = Nothing
    , tags = Nothing
    , timestamp
    , transaction = Nothing
    }
