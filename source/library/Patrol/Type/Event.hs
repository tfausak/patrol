module Patrol.Type.Event where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
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
import qualified Patrol.Type.Error as Error
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Exception as Exception
import qualified Patrol.Type.Level as Level
import qualified Patrol.Type.Platform as Platform

data Event = Event
  { dist :: Maybe Text.Text,
    environment :: Maybe Text.Text,
    errors :: [Error.Error],
    eventId :: EventId.EventId,
    exception :: [Exception.Exception],
    extra :: Map.Map Text.Text Aeson.Value,
    fingerprint :: [Text.Text],
    level :: Maybe Level.Level,
    logger :: Maybe Text.Text,
    modules :: Map.Map Text.Text Text.Text,
    platform :: Maybe Platform.Platform,
    release :: Maybe Text.Text,
    serverName :: Maybe Text.Text,
    tags :: Map.Map Text.Text Text.Text,
    timestamp :: Maybe Time.UTCTime,
    transaction :: Maybe Text.Text
    -- TODO: Add more fields.
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Event where
  toJSON event =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "dist" Aeson..= dist event,
          Key.fromString "environment" Aeson..= environment event,
          Key.fromString "errors" Aeson..= errors event,
          Key.fromString "exception" Aeson..= exception event,
          Key.fromString "extra" Aeson..= extra event,
          Key.fromString "event_id" Aeson..= eventId event,
          Key.fromString "fingerprint" Aeson..= fingerprint event,
          Key.fromString "level" Aeson..= level event,
          Key.fromString "logger" Aeson..= logger event,
          Key.fromString "modules" Aeson..= modules event,
          Key.fromString "platform" Aeson..= platform event,
          Key.fromString "release" Aeson..= release event,
          Key.fromString "server_name" Aeson..= serverName event,
          Key.fromString "tags" Aeson..= tags event,
          Key.fromString "timestamp" Aeson..= timestamp event,
          Key.fromString "transaction" Aeson..= transaction event
        ]

new :: IO.MonadIO io => io Event
new = do
  theEventId <- EventId.random
  theTimestamp <- IO.liftIO Time.getCurrentTime
  pure
    Event
      { dist = Nothing,
        environment = Just $ Text.pack "production",
        errors = [],
        eventId = theEventId,
        exception = [],
        extra = Map.empty,
        fingerprint = [],
        level = Just Level.Error,
        logger = Nothing,
        modules = Map.empty,
        platform = Just Platform.Haskell,
        release = Nothing,
        serverName = Nothing,
        tags = Map.empty,
        timestamp = Just theTimestamp,
        transaction = Nothing
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
