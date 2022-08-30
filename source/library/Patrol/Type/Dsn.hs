module Patrol.Type.Dsn where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Network.URI as Uri
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

fromUri :: Uri.URI -> Maybe Dsn
fromUri uri = do
  theProtocol <- do
    text <- Text.stripSuffix (Text.singleton ':') . Text.pack $ Uri.uriScheme uri
    Protocol.fromText text
  uriAuth <- Uri.uriAuthority uri
  userInfo <- Text.stripSuffix (Text.singleton '@') . Text.pack $ Uri.uriUserInfo uriAuth
  let (user, pass) = fmap (Text.drop 1) $ Text.breakOn (Text.singleton ':') userInfo
  thePublicKey <- PublicKey.fromText user
  maybeSecretKey <- if Text.null pass then pure Nothing else fmap pure $ SecretKey.fromText pass
  theHost <- Host.fromText . Text.pack $ Uri.uriRegName uriAuth
  maybePort <- case Text.stripPrefix (Text.singleton ':') . Text.pack $ Uri.uriPort uriAuth of
    Nothing -> pure Nothing
    Just text -> fmap (pure . Port.fromNatural) . Read.readMaybe $ Text.unpack text
  let (before, after) = Text.breakOnEnd (Text.singleton '/') . Text.pack $ Uri.uriPath uri
  thePath <- Path.fromText before
  theProjectId <- ProjectId.fromText after
  Monad.guard . null $ Uri.uriQuery uri
  Monad.guard . null $ Uri.uriFragment uri
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

toUri :: Dsn -> Uri.URI
toUri dsn =
  Uri.URI
    { Uri.uriScheme = mconcat [Text.unpack . Protocol.toText $ protocol dsn, ":"],
      Uri.uriAuthority =
        Just
          Uri.URIAuth
            { Uri.uriUserInfo =
                mconcat
                  [ Text.unpack . PublicKey.toText $ publicKey dsn,
                    case secretKey dsn of
                      Nothing -> ""
                      Just x -> mconcat [":", Text.unpack $ SecretKey.toText x],
                    "@"
                  ],
              Uri.uriRegName = Text.unpack . Host.toText $ host dsn,
              Uri.uriPort = case port dsn of
                Nothing -> ""
                Just x -> mconcat [":", show $ Port.toNatural x]
            },
      Uri.uriPath =
        mconcat
          [ Text.unpack . Path.toText $ path dsn,
            Text.unpack . ProjectId.toText $ projectId dsn
          ],
      Uri.uriQuery = "",
      Uri.uriFragment = ""
    }
