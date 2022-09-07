module Patrol.Type.Dsn where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.URI as Uri
import qualified Patrol.Constant as Constant
import qualified Patrol.Exception.Problem as Problem
import qualified Patrol.Type.Host as Host
import qualified Patrol.Type.Path as Path
import qualified Patrol.Type.Port as Port
import qualified Patrol.Type.ProjectId as ProjectId
import qualified Patrol.Type.Protocol as Protocol
import qualified Patrol.Type.PublicKey as PublicKey
import qualified Patrol.Type.SecretKey as SecretKey
import qualified Text.Read as Read

data Dsn = Dsn
  { protocol :: Protocol.Protocol,
    publicKey :: PublicKey.PublicKey,
    secretKey :: Maybe SecretKey.SecretKey,
    host :: Host.Host,
    port :: Maybe Port.Port,
    path :: Path.Path,
    projectId :: ProjectId.ProjectId
  }
  deriving (Eq, Show)

fromUri :: Catch.MonadThrow m => Uri.URI -> m Dsn
fromUri uri = do
  theProtocol <- do
    text <- maybe (Catch.throwM $ Problem.Problem "invalid scheme") pure . Text.stripSuffix (Text.singleton ':') . Text.pack $ Uri.uriScheme uri
    Protocol.fromText text
  uriAuth <- maybe (Catch.throwM $ Problem.Problem "missing authority") pure $ Uri.uriAuthority uri
  userInfo <- maybe (Catch.throwM $ Problem.Problem "invalid user information") pure . Text.stripSuffix (Text.singleton '@') . Text.pack $ Uri.uriUserInfo uriAuth
  let (user, pass) = fmap (Text.drop 1) $ Text.breakOn (Text.singleton ':') userInfo
  thePublicKey <- PublicKey.fromText user
  maybeSecretKey <- if Text.null pass then pure Nothing else fmap Just $ SecretKey.fromText pass
  theHost <- Host.fromText . Text.pack $ Uri.uriRegName uriAuth
  maybePort <- case Text.stripPrefix (Text.singleton ':') . Text.pack $ Uri.uriPort uriAuth of
    Nothing -> pure Nothing
    Just text -> maybe (Catch.throwM $ Problem.Problem "invalid port") (pure . Just . Port.fromNatural) . Read.readMaybe $ Text.unpack text
  let (before, after) = Text.breakOnEnd (Text.singleton '/') . Text.pack $ Uri.uriPath uri
  thePath <- Path.fromText before
  theProjectId <- ProjectId.fromText after
  Monad.unless (null $ Uri.uriQuery uri) . Catch.throwM $ Problem.Problem "unexpected query"
  Monad.unless (null $ Uri.uriFragment uri) . Catch.throwM $ Problem.Problem "unexpected fragment"
  pure
    Dsn
      { protocol = theProtocol,
        publicKey = thePublicKey,
        secretKey = maybeSecretKey,
        host = theHost,
        port = maybePort,
        path = thePath,
        projectId = theProjectId
      }

intoUri :: Dsn -> Uri.URI
intoUri dsn =
  Uri.URI
    { Uri.uriScheme = mconcat [Text.unpack . Protocol.intoText $ protocol dsn, ":"],
      Uri.uriAuthority =
        Just
          Uri.URIAuth
            { Uri.uriUserInfo =
                mconcat
                  [ Text.unpack . PublicKey.intoText $ publicKey dsn,
                    case secretKey dsn of
                      Nothing -> ""
                      Just x -> mconcat [":", Text.unpack $ SecretKey.intoText x],
                    "@"
                  ],
              Uri.uriRegName = Text.unpack . Host.intoText $ host dsn,
              Uri.uriPort = case port dsn of
                Nothing -> ""
                Just x -> mconcat [":", show $ Port.intoNatural x]
            },
      Uri.uriPath =
        mconcat
          [ Text.unpack . Path.intoText $ path dsn,
            Text.unpack . ProjectId.intoText $ projectId dsn
          ],
      Uri.uriQuery = "",
      Uri.uriFragment = ""
    }

intoAuthorization :: Dsn -> ByteString.ByteString
intoAuthorization dsn =
  Text.encodeUtf8
    . (Text.pack "Sentry " <>)
    . Text.intercalate (Text.singleton ',')
    $ Maybe.mapMaybe
      (\(k, m) -> fmap (\v -> Text.pack k <> Text.singleton '=' <> v) m)
      [ ("sentry_version", Just Constant.sentryVersion),
        ("sentry_client", Just Constant.userAgent),
        ("sentry_key", Just . PublicKey.intoText $ publicKey dsn),
        ("sentry_secret", fmap SecretKey.intoText $ secretKey dsn)
      ]
