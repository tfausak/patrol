module Patrol.Type.Event where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Patrol.Constant as Constant
import qualified Patrol.Extra.List as List
import qualified Patrol.Type.Dist as Dist
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.Environment as Environment
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Host as Host
import qualified Patrol.Type.Level as Level
import qualified Patrol.Type.Logger as Logger
import qualified Patrol.Type.ModuleName as ModuleName
import qualified Patrol.Type.ModuleVersion as ModuleVersion
import qualified Patrol.Type.Path as Path
import qualified Patrol.Type.Platform as Platform
import qualified Patrol.Type.Port as Port
import qualified Patrol.Type.ProjectId as ProjectId
import qualified Patrol.Type.Protocol as Protocol
import qualified Patrol.Type.Release as Release
import qualified Patrol.Type.ServerName as ServerName
import qualified Patrol.Type.TagKey as TagKey
import qualified Patrol.Type.TagValue as TagValue
import qualified Patrol.Type.Timestamp as Timestamp
import qualified Patrol.Type.Transaction as Transaction

data Event = Event
  { dist :: Maybe Dist.Dist,
    environment :: Maybe Environment.Environment,
    extra :: Maybe (Map.Map Text.Text Aeson.Value),
    fingerprint :: Maybe [Text.Text],
    id :: EventId.EventId,
    level :: Maybe Level.Level,
    logger :: Maybe Logger.Logger,
    modules :: Maybe (Map.Map ModuleName.ModuleName ModuleVersion.ModuleVersion),
    platform :: Maybe Platform.Platform,
    release :: Maybe Release.Release,
    serverName :: Maybe ServerName.ServerName,
    tags :: Maybe (Map.Map TagKey.TagKey TagValue.TagValue),
    timestamp :: Maybe Timestamp.Timestamp,
    transaction :: Maybe Transaction.Transaction
    -- TODO: Add more fields.
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Event where
  toJSON event =
    Aeson.object $
      filter
        ((/=) Aeson.Null . snd)
        [ Key.fromString "dist" Aeson..= dist event,
          Key.fromString "environment" Aeson..= environment event,
          Key.fromString "extra" Aeson..= extra event,
          Key.fromString "event_id" Aeson..= Patrol.Type.Event.id event,
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
  theId <- EventId.random
  theTimestamp <- Timestamp.now
  pure
    Event
      { dist = Nothing,
        environment = Just Environment.production,
        extra = Nothing,
        fingerprint = Nothing,
        Patrol.Type.Event.id = theId,
        level = Just Level.Error,
        logger = Nothing,
        modules = Nothing,
        platform = Just Platform.Haskell,
        release = Nothing,
        serverName = Nothing,
        tags = Nothing,
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
