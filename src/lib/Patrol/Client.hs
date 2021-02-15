module Patrol.Client
  ( store
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.CaseInsensitive as CI
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Paths_patrol as Package
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.Event as Event
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Type.Response as Response

-- TODO: Handle 429 response codes.
-- TODO: Compress request body.
store :: Client.Manager -> Dsn.Dsn -> Event.Event -> IO EventId.EventId
store manager dsn event = do
  now <- Time.getCurrentTime
  request <- Client.parseUrlThrow $ makeUrl dsn
  response <- Client.httpLbs request
    { Client.requestBody = Client.RequestBodyLBS $ Aeson.encode event
    , Client.requestHeaders =
      [ (Http.hContentType, utf8 "application/json")
      , (Http.hUserAgent, utf8 userAgent)
      , (ci $ utf8 "X-Sentry-Auth", Text.encodeUtf8 . Text.intercalate (Text.singleton ',') $ Maybe.catMaybes
        [ Just $ Text.pack "Sentry sentry_version=7"
        , Just . Text.pack $ "sentry_client=" <> userAgent
        , Just . Text.pack $ "sentry_timestamp=" <> Time.formatTime Time.defaultTimeLocale "%s" now
        , Just $ Text.pack "sentry_key=" <> Dsn.publicKey dsn
        , (\ x -> Text.pack "sentry_secret=" <> x) <$> Dsn.secretKey dsn
        ])
      ]
    , Client.method = Http.methodPost
    } manager
  either fail (pure . Response.id) . Aeson.eitherDecode $ Client.responseBody response

makeUrl :: Dsn.Dsn -> String
makeUrl dsn =
  Text.unpack (Dsn.protocol dsn)
  <> "://"
  <> Text.unpack (Dsn.host dsn)
  <> maybe "" (\ x -> ":" <> Text.unpack x) (Dsn.port dsn)
  <> Text.unpack (Dsn.path dsn)
  <> "api/"
  <> Text.unpack (Dsn.projectId dsn)
  <> "/store/"

utf8 :: String -> ByteString.ByteString
utf8 = Text.encodeUtf8 . Text.pack

ci :: CI.FoldCase a => a -> CI.CI a
ci = CI.mk

userAgent :: String
userAgent = "patrol/" <> Version.showVersion Package.version
