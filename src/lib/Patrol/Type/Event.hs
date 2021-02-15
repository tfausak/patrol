module Patrol.Type.Event
  ( Event(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Level as Level
import qualified Patrol.Type.Platform as Platform
import qualified Patrol.Type.Timestamp as Timestamp
import qualified Patrol.Utility.Json as Json

data Event = Event
  { dist :: Maybe Text.Text
  , environment :: Maybe Text.Text
  , eventId :: EventId.EventId
  , extra :: Maybe Aeson.Object
  , fingerprint :: Maybe [Text.Text]
  , level :: Maybe Level.Level
  , logger :: Maybe Text.Text
  , modules :: Maybe (Map.Map Text.Text Text.Text)
  , platform :: Platform.Platform
  , release :: Maybe Text.Text
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
    , Json.pair "fingerprint" <$> fingerprint event
    , Json.pair "level" <$> level event
    , Json.pair "logger" <$> logger event
    , Json.pair "modules" <$> modules event
    , Just . Json.pair "platform" $ platform event
    , Json.pair "release" <$> release event
    , Json.pair "server_name" <$> serverName event
    , Json.pair "tags" <$> tags event
    , Just . Json.pair "timestamp" $ timestamp event
    , Json.pair "transaction" <$> transaction event
    ]
