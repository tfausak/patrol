module Patrol.Type.Dsn
  ( Dsn (..),
    fromUri,
    fromString,
    toUri,
    toString,
  )
where

import qualified Data.Text as Text
import qualified Network.URI as Uri
import qualified Patrol.Utility.Maybe as Maybe

-- | <https://develop.sentry.dev/sdk/overview/#parsing-the-dsn>
data Dsn = Dsn
  { protocol :: Text.Text,
    publicKey :: Text.Text,
    secretKey :: Maybe Text.Text,
    host :: Text.Text,
    port :: Maybe Text.Text,
    path :: Text.Text,
    projectId :: Text.Text
  }
  deriving (Eq, Show)

fromUri :: Uri.URI -> Either String Dsn
fromUri uri = do
  protocol <-
    Maybe.note "invalid protocol"
      . Text.stripSuffix (Text.singleton ':')
      . Text.pack
      $ Uri.uriScheme uri
  authority <- Maybe.note "missing authority" $ Uri.uriAuthority uri
  userInfo <-
    Maybe.note "invalid user info"
      . Text.stripSuffix (Text.singleton '@')
      . Text.pack
      $ Uri.uriUserInfo authority
  let (publicKey, secretKey) =
        Text.drop 1
          <$> Text.breakOn (Text.singleton ':') userInfo
      (host, port) =
        fmap (Text.drop 1)
          . Text.breakOn (Text.singleton ':')
          . Text.pack
          $ Uri.uriRegName authority <> Uri.uriPort authority
      (path, projectId) =
        Text.breakOnEnd (Text.singleton '/')
          . Text.pack
          $ Uri.uriPath uri
  Right
    Dsn
      { protocol,
        publicKey,
        secretKey = if Text.null secretKey then Nothing else Just secretKey,
        host,
        port = if Text.null port then Nothing else Just port,
        path,
        projectId
      }

fromString :: String -> Either String Dsn
fromString string = do
  uri <- Maybe.note "invalid URI" $ Uri.parseURI string
  fromUri uri

toUri :: Dsn -> Uri.URI
toUri dsn =
  Uri.URI
    { Uri.uriScheme = Text.unpack (protocol dsn) <> ":",
      Uri.uriAuthority =
        Just
          Uri.URIAuth
            { Uri.uriUserInfo =
                Text.unpack (publicKey dsn)
                  <> maybe "" (\x -> ":" <> Text.unpack x) (secretKey dsn)
                  <> "@",
              Uri.uriRegName =
                Text.unpack (host dsn)
                  <> maybe "" (\x -> ":" <> Text.unpack x) (port dsn),
              Uri.uriPort = ""
            },
      Uri.uriPath = Text.unpack (path dsn) <> Text.unpack (projectId dsn),
      Uri.uriQuery = "",
      Uri.uriFragment = ""
    }

toString :: Dsn -> String
toString dsn = Uri.uriToString id (toUri dsn) ""
