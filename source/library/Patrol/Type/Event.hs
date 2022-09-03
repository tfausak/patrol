module Patrol.Type.Event where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Patrol.Constant as Constant
import qualified Patrol.Extra.List as List
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Host as Host
import qualified Patrol.Type.Path as Path
import qualified Patrol.Type.Port as Port
import qualified Patrol.Type.ProjectId as ProjectId
import qualified Patrol.Type.Protocol as Protocol
import qualified Patrol.Type.Timestamp as Timestamp

data Event = Event
  { id :: EventId.EventId,
    timestamp :: Maybe Timestamp.Timestamp
    -- TODO: Add more fields.
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Event where
  toJSON event =
    Aeson.object $
      filter
        ((/=) Aeson.Null . snd)
        [ Key.fromString "event_id" Aeson..= Patrol.Type.Event.id event,
          Key.fromString "timestamp" Aeson..= timestamp event
        ]

new :: IO.MonadIO io => io Event
new = do
  theId <- EventId.random
  now <- IO.liftIO Time.getCurrentTime
  pure
    Event
      { Patrol.Type.Event.id = theId,
        timestamp = Just $ Timestamp.fromUtcTime now
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
