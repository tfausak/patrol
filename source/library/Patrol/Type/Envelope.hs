module Patrol.Type.Envelope where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified GHC.Stack as Stack
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Patrol.Constant as Constant
import qualified Patrol.Extra.List as List
import qualified Patrol.Type.ClientSdkInfo as ClientSdkInfo
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.Event as Event
import qualified Patrol.Type.Headers as Headers
import qualified Patrol.Type.Item as Item

-- | <https://develop.sentry.dev/sdk/data-model/envelopes/>
data Envelope = Envelope
  { headers :: Headers.Headers,
    items :: [Item.Item]
  }
  deriving (Eq, Show)

fromException ::
  (Catch.Exception e, IO.MonadIO io) =>
  (Catch.SomeException -> Maybe Stack.CallStack) ->
  Dsn.Dsn ->
  e ->
  io Envelope
fromException getCallStack dsn =
  fmap (fromEvent dsn)
    . Event.fromException getCallStack

fromEvent :: Dsn.Dsn -> Event.Event -> Envelope
fromEvent dsn event =
  Envelope
    { headers =
        Headers.fromObject
          . KeyMap.fromList
          $ Maybe.catMaybes
            [ Just (Key.fromString "dsn", Aeson.toJSON $ Dsn.intoUri dsn),
              Just (Key.fromString "sdk", Aeson.toJSON ClientSdkInfo.patrol),
              (,) (Key.fromString "sent_at") . Aeson.toJSON <$> Event.timestamp event
            ],
      items = [Item.fromEvent event]
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
    <> foldMap ((<> Builder.char7 '\n') . Item.serialize) (items envelope)
