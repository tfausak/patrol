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
import qualified Patrol.Type.Breadcrumbs as Breadcrumbs
import qualified Patrol.Type.ClientSdkInfo as ClientSdkInfo
import qualified Patrol.Type.DebugMeta as DebugMeta
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.EventProcessingError as EventProcessingError
import qualified Patrol.Type.EventType as EventType
import qualified Patrol.Type.Exceptions as Exceptions
import qualified Patrol.Type.Level as Level
import qualified Patrol.Type.LogEntry as LogEntry
import qualified Patrol.Type.Platform as Platform
import qualified Patrol.Type.Request as Request
import qualified Patrol.Type.Threads as Threads
import qualified Patrol.Type.TransactionInfo as TransactionInfo
import qualified Patrol.Type.User as User

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#event>
data Event = Event
  { breadcrumbs :: Maybe Breadcrumbs.Breadcrumbs,
    -- TODO: contexts
    debugMeta :: Maybe DebugMeta.DebugMeta,
    dist :: Maybe Text.Text,
    environment :: Maybe Text.Text,
    errors :: [EventProcessingError.EventProcessingError],
    eventId :: EventId.EventId,
    exception :: Maybe Exceptions.Exceptions,
    extra :: Map.Map Text.Text Aeson.Value,
    fingerprint :: [Text.Text],
    level :: Maybe Level.Level,
    logentry :: Maybe LogEntry.LogEntry,
    logger :: Maybe Text.Text,
    modules :: Map.Map Text.Text (Maybe Text.Text),
    platform :: Maybe Platform.Platform,
    release :: Maybe Text.Text,
    request :: Maybe Request.Request,
    sdk :: Maybe ClientSdkInfo.ClientSdkInfo,
    serverName :: Maybe Text.Text,
    tags :: Map.Map Text.Text (Maybe Text.Text),
    threads :: Maybe Threads.Threads,
    timeSpent :: Maybe Time.NominalDiffTime,
    timestamp :: Maybe Time.UTCTime,
    transaction :: Maybe Text.Text,
    transactionInfo :: Maybe TransactionInfo.TransactionInfo,
    type_ :: Maybe EventType.EventType,
    user :: Maybe User.User,
    version :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Event where
  toJSON event =
    Aeson.intoObject
      [ Aeson.pair "breadcrumbs" $ breadcrumbs event,
        Aeson.pair "debug_meta" $ debugMeta event,
        Aeson.pair "dist" $ dist event,
        Aeson.pair "environment" $ environment event,
        Aeson.pair "errors" $ errors event,
        Aeson.pair "exception" $ exception event,
        Aeson.pair "extra" $ extra event,
        Aeson.pair "event_id" $ eventId event,
        Aeson.pair "fingerprint" $ fingerprint event,
        Aeson.pair "level" $ level event,
        Aeson.pair "logentry" $ logentry event,
        Aeson.pair "logger" $ logger event,
        Aeson.pair "modules" $ modules event,
        Aeson.pair "platform" $ platform event,
        Aeson.pair "release" $ release event,
        Aeson.pair "request" $ request event,
        Aeson.pair "sdk" $ sdk event,
        Aeson.pair "server_name" $ serverName event,
        Aeson.pair "tags" $ tags event,
        Aeson.pair "threads" $ threads event,
        Aeson.pair "time_spent" $ timeSpent event,
        Aeson.pair "timestamp" $ timestamp event,
        Aeson.pair "transaction" $ transaction event,
        Aeson.pair "transaction_info" $ transactionInfo event,
        Aeson.pair "type" $ type_ event,
        Aeson.pair "user" $ user event,
        Aeson.pair "version" $ version event
      ]

empty :: Event
empty =
  Event
    { breadcrumbs = Nothing,
      debugMeta = Nothing,
      dist = Nothing,
      environment = Nothing,
      errors = [],
      eventId = EventId.empty,
      exception = Nothing,
      extra = Map.empty,
      fingerprint = [],
      level = Nothing,
      logentry = Nothing,
      logger = Nothing,
      modules = Map.empty,
      platform = Nothing,
      release = Nothing,
      request = Nothing,
      sdk = Nothing,
      serverName = Nothing,
      tags = Map.empty,
      threads = Nothing,
      timeSpent = Nothing,
      timestamp = Nothing,
      transaction = Nothing,
      transactionInfo = Nothing,
      type_ = Nothing,
      user = Nothing,
      version = Nothing
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
        timestamp = Just theTimestamp,
        type_ = Just EventType.Default,
        version = Just Constant.sentryVersion
      }

intoRequest :: Catch.MonadThrow m => Dsn.Dsn -> Event -> m Client.Request
intoRequest dsn event = do
  theRequest <-
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
  let oldHeaders = Client.requestHeaders theRequest
      authorization = Dsn.intoAuthorization dsn
      newHeaders =
        [ (Http.hContentType, Constant.applicationJson),
          (Http.hUserAgent, Text.encodeUtf8 Constant.userAgent),
          (Constant.xSentryAuth, authorization)
        ]
  pure
    theRequest
      { Client.method = Http.methodPost,
        Client.requestBody = Client.RequestBodyBS . LazyByteString.toStrict $ Aeson.encode event,
        Client.requestHeaders = List.insertAll newHeaders oldHeaders
      }
