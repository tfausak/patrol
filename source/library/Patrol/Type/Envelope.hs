module Patrol.Type.Envelope where

import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Patrol.Constant as Constant
import qualified Patrol.Extra.List as List
import qualified Patrol.Type.ClientSdkInfo as ClientSdkInfo
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.Event as Event
import qualified Patrol.Type.Headers as Headers
import qualified Patrol.Type.Item as Item
import qualified Patrol.Type.Items as Items

-- | <https://develop.sentry.dev/sdk/data-model/envelopes/>
data Envelope = Envelope
  { headers :: Headers.Headers,
    items :: Items.Items
  }
  deriving (Eq, Show)

fromEvent :: Dsn.Dsn -> Event.Event -> Envelope
fromEvent dsn event =
  Envelope
    { headers = Headers.empty
        { Headers.eventId = Just $ Event.eventId event,
          Headers.dsn = Just dsn,
          Headers.sdk = Just ClientSdkInfo.patrol,
          Headers.sentAt = Event.timestamp event
        },
      items = Items.EnvelopeItems [Item.Event event]
    }

intoRequest :: (Catch.MonadThrow m) => Dsn.Dsn -> Envelope -> m Client.Request
intoRequest dsn envelope = do
  request <-
    Client.parseUrlThrow $
      mconcat
        [ Text.unpack $ Dsn.protocol dsn,
          "://",
          Text.unpack $ Dsn.host dsn,
          maybe "" ((':' :) . show) $ Dsn.port dsn,
          Text.unpack $ Dsn.path dsn,
          "api/",
          Text.unpack $ Dsn.projectId dsn,
          "/envelope/"
        ]
  let body =
        LazyByteString.toStrict
          . Builder.toLazyByteString
          $ serialize envelope
  pure
    request
      { Client.method = Http.methodPost,
        Client.requestHeaders =
          List.insertAll
            [ (Http.hContentLength, Encoding.encodeUtf8 . Text.pack . show $ ByteString.length body),
              (Http.hContentType, Constant.applicationXSentryEnvelope),
              (Http.hUserAgent, Encoding.encodeUtf8 Constant.userAgent),
              (Constant.xSentryAuth, Dsn.intoAuthorization dsn)
            ]
            $ Client.requestHeaders request,
        Client.requestBody = Client.RequestBodyBS body
      }

serialize :: Envelope -> Builder.Builder
serialize envelope =
  Headers.serialize (headers envelope)
    <> Builder.char7 '\n'
    <> Items.serialize (items envelope)
