module Patrol.Type.Dsn where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.URI as Uri
import qualified Numeric.Natural as Natural
import qualified Patrol.Constant as Constant
import qualified Patrol.Exception.Problem as Problem
import qualified Text.Read as Read

data Dsn = Dsn
  { protocol :: Text.Text,
    publicKey :: Text.Text,
    secretKey :: Maybe Text.Text,
    host :: Text.Text,
    port :: Maybe Natural.Natural,
    path :: Text.Text,
    projectId :: Text.Text
  }
  deriving (Eq, Show)

fromUri :: Catch.MonadThrow m => Uri.URI -> m Dsn
fromUri uri = do
  theProtocol <- maybe (Catch.throwM $ Problem.Problem "invalid scheme") pure . Text.stripSuffix (Text.singleton ':') . Text.pack $ Uri.uriScheme uri
  uriAuth <- maybe (Catch.throwM $ Problem.Problem "missing authority") pure $ Uri.uriAuthority uri
  userInfo <- maybe (Catch.throwM $ Problem.Problem "invalid user information") pure . Text.stripSuffix (Text.singleton '@') . Text.pack $ Uri.uriUserInfo uriAuth
  let (thePublicKey, pass) = fmap (Text.drop 1) $ Text.breakOn (Text.singleton ':') userInfo
      maybeSecretKey = if Text.null pass then Nothing else Just pass
      theHost = Text.pack $ Uri.uriRegName uriAuth
  maybePort <- case Text.stripPrefix (Text.singleton ':') . Text.pack $ Uri.uriPort uriAuth of
    Nothing -> pure Nothing
    Just text -> maybe (Catch.throwM $ Problem.Problem "invalid port") (pure . Just) . Read.readMaybe $ Text.unpack text
  let (thePath, theProjectId) = Text.breakOnEnd (Text.singleton '/') . Text.pack $ Uri.uriPath uri
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
    { Uri.uriScheme = mconcat [Text.unpack $ protocol dsn, ":"],
      Uri.uriAuthority =
        Just
          Uri.URIAuth
            { Uri.uriUserInfo =
                mconcat
                  [ Text.unpack $ publicKey dsn,
                    case secretKey dsn of
                      Nothing -> ""
                      Just x -> mconcat [":", Text.unpack x],
                    "@"
                  ],
              Uri.uriRegName = Text.unpack $ host dsn,
              Uri.uriPort = case port dsn of
                Nothing -> ""
                Just x -> mconcat [":", show x]
            },
      Uri.uriPath =
        mconcat
          [ Text.unpack $ path dsn,
            Text.unpack $ projectId dsn
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
        ("sentry_key", Just $ publicKey dsn),
        ("sentry_secret", secretKey dsn)
      ]
