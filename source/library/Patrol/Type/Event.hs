module Patrol.Type.Event where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Patrol.Constant as Constant
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Extra.List as List
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.EventProcessingError as EventProcessingError
import qualified Patrol.Type.Exceptions as Exceptions
import qualified Patrol.Type.Level as Level
import qualified Patrol.Type.Platform as Platform

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#event>
data Event = Event
  { -- TODO: breadcrumbs
    -- TODO: contexts
    -- TODO: debug_meta
    dist :: Maybe Text.Text,
    environment :: Maybe Text.Text,
    errors :: [EventProcessingError.EventProcessingError],
    eventId :: EventId.EventId,
    exception :: Maybe Exceptions.Exceptions,
    extra :: Map.Map Text.Text Aeson.Value,
    fingerprint :: [Text.Text],
    level :: Maybe Level.Level,
    -- TODO: logentry
    logger :: Maybe Text.Text,
    modules :: Map.Map Text.Text (Maybe Text.Text),
    platform :: Maybe Platform.Platform,
    release :: Maybe Text.Text,
    -- TODO: request
    -- TODO: sdk
    serverName :: Maybe Text.Text,
    tags :: Map.Map Text.Text (Maybe Text.Text),
    -- TODO: threads
    -- TODO: time_spent
    timestamp :: Maybe Time.UTCTime,
    transaction :: Maybe Text.Text
    -- TODO: transaction_info
    -- TODO: type ("transaction" only)
    -- TODO: user
    -- TODO: version (always "7")
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Event where
  toJSON event =
    Aeson.intoObject
      [ Aeson.pair "dist" $ dist event,
        Aeson.pair "environment" $ environment event,
        Aeson.pair "errors" $ errors event,
        Aeson.pair "exception" $ exception event,
        Aeson.pair "extra" $ extra event,
        Aeson.pair "event_id" $ eventId event,
        Aeson.pair "fingerprint" $ fingerprint event,
        Aeson.pair "level" $ level event,
        Aeson.pair "logger" $ logger event,
        Aeson.pair "modules" $ modules event,
        Aeson.pair "platform" $ platform event,
        Aeson.pair "release" $ release event,
        Aeson.pair "server_name" $ serverName event,
        Aeson.pair "tags" $ tags event,
        Aeson.pair "timestamp" $ timestamp event,
        Aeson.pair "transaction" $ transaction event
      ]

empty :: Event
empty =
  Event
    { dist = Nothing,
      environment = Nothing,
      errors = [],
      eventId = EventId.empty,
      exception = Nothing,
      extra = Map.empty,
      fingerprint = [],
      level = Nothing,
      logger = Nothing,
      modules = Map.empty,
      platform = Nothing,
      release = Nothing,
      serverName = Nothing,
      tags = Map.empty,
      timestamp = Nothing,
      transaction = Nothing
    }

new :: IO.MonadIO io => io Event
new = do
  theEventId <- EventId.random
  theTimestamp <- IO.liftIO Time.getCurrentTime
  pure
    empty
      { environment = Just $ Text.pack "production",
        eventId = theEventId,
        level = Just Level.Error,
        platform = Just Platform.Haskell,
        timestamp = Just theTimestamp
      }

intoRequest :: Catch.MonadThrow m => Dsn.Dsn -> Event -> m Client.Request
intoRequest dsn event = do
  request <-
    Client.parseUrlThrow
      . Text.unpack
      $ mconcat
        [ Dsn.protocol dsn,
          Text.pack "://",
          Dsn.host dsn,
          maybe Text.empty (Text.pack . (:) ':' . show) $ Dsn.port dsn,
          Dsn.path dsn,
          Text.pack "api/",
          Dsn.projectId dsn,
          Text.pack "/store/"
        ]
  let oldHeaders = Client.requestHeaders request
      authorization = Dsn.intoAuthorization dsn
      newHeaders =
        [ (Http.hContentType, Constant.applicationJson),
          (Http.hUserAgent, Text.encodeUtf8 Constant.userAgent),
          (Constant.xSentryAuth, authorization)
        ]
  pure
    request
      { Client.method = Http.methodPost,
        Client.requestBody = Client.RequestBodyBS . LazyByteString.toStrict $ Aeson.encode event,
        Client.requestHeaders = List.insertAll newHeaders oldHeaders
      }
