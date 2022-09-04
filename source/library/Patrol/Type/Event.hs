module Patrol.Type.Event where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Patrol.Constant as Constant
import qualified Patrol.Extra.List as List
import qualified Patrol.Type.Dist as Dist
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Host as Host
import qualified Patrol.Type.Level as Level
import qualified Patrol.Type.Path as Path
import qualified Patrol.Type.Platform as Platform
import qualified Patrol.Type.Port as Port
import qualified Patrol.Type.ProjectId as ProjectId
import qualified Patrol.Type.Protocol as Protocol
import qualified Patrol.Type.Timestamp as Timestamp

data Event = Event
  { dist :: Maybe Dist.Dist,
    id :: EventId.EventId,
    level :: Maybe Level.Level,
    logger :: Maybe Text.Text,
    platform :: Maybe Platform.Platform,
    release :: Maybe Text.Text,
    serverName :: Maybe Text.Text,
    timestamp :: Maybe Timestamp.Timestamp,
    transaction :: Maybe Text.Text
    -- TODO: Add more fields.
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Event where
  toJSON event =
    Aeson.object $
      filter
        ((/=) Aeson.Null . snd)
        [ Key.fromString "dist" Aeson..= dist event,
          Key.fromString "event_id" Aeson..= Patrol.Type.Event.id event,
          Key.fromString "level" Aeson..= level event,
          Key.fromString "logger" Aeson..= logger event,
          Key.fromString "platform" Aeson..= platform event,
          Key.fromString "release" Aeson..= release event,
          Key.fromString "server_name" Aeson..= serverName event,
          Key.fromString "timestamp" Aeson..= timestamp event,
          Key.fromString "transaction" Aeson..= transaction event
        ]

new :: IO.MonadIO io => io Event
new = do
  theId <- EventId.random
  theTimestamp <- Timestamp.now
  pure
    Event
      { dist = Nothing,
        Patrol.Type.Event.id = theId,
        level = Just Level.Error,
        logger = Nothing,
        platform = Just Platform.Haskell,
        release = Nothing,
        serverName = Nothing,
        timestamp = Just theTimestamp,
        transaction = Nothing
      }

intoRequest :: Exception.MonadThrow m => Dsn.Dsn -> Event -> m Client.Request
intoRequest dsn event = do
  request <-
    Client.parseUrlThrow
      . Text.unpack
      $ mconcat
        [ Protocol.intoText $ Dsn.protocol dsn,
          Text.pack "://",
          Host.intoText $ Dsn.host dsn,
          maybe Text.empty (Text.pack . (:) ':' . show . Port.intoNatural) $ Dsn.port dsn,
          Path.intoText $ Dsn.path dsn,
          Text.pack "api/",
          ProjectId.intoText $ Dsn.projectId dsn,
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
