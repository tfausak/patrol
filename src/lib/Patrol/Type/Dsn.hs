module Patrol.Type.Dsn
  ( Dsn(..)
  , fromUri
  , fromString
  , toUri
  , toString
  ) where

import qualified Data.Text as Text
import qualified Network.URI as Uri
import qualified Patrol.Utility.Maybe as Maybe

data Dsn = Dsn
  { protocol :: Text.Text
  , publicKey :: Text.Text
  , secretKey :: Maybe Text.Text
  , host :: Text.Text
  , port :: Maybe Text.Text
  , path :: Text.Text
  , projectId :: Text.Text
  } deriving (Eq, Show)

fromUri :: Uri.URI -> Either String Dsn
fromUri uri = do
  theProtocol <- Maybe.note "invalid protocol"
    . Text.stripSuffix (Text.singleton ':')
    . Text.pack
    $ Uri.uriScheme uri
  authority <- Maybe.note "missing authority" $ Uri.uriAuthority uri
  userInfo <- Maybe.note "invalid user info"
    . Text.stripSuffix (Text.singleton '@')
    . Text.pack
    $ Uri.uriUserInfo authority
  let
    (thePublicKey, theSecretKey) = Text.drop 1
      <$> Text.breakOn (Text.singleton ':') userInfo
    (theHost, thePort) = fmap (Text.drop 1) . Text.breakOn (Text.singleton ':')
      . Text.pack
      $ Uri.uriRegName authority <> Uri.uriPort authority
    (thePath, theProjectId) = Text.breakOnEnd (Text.singleton '/')
      . Text.pack
      $ Uri.uriPath uri
  Right Dsn
    { protocol = theProtocol
    , publicKey = thePublicKey
    , secretKey = if Text.null theSecretKey then Nothing else Just theSecretKey
    , host = theHost
    , port = if Text.null thePort then Nothing else Just thePort
    , path = thePath
    , projectId = theProjectId
    }

fromString :: String -> Either String Dsn
fromString string = do
  uri <- Maybe.note "invalid URI" $ Uri.parseURI string
  fromUri uri

toUri :: Dsn -> Uri.URI
toUri dsn = Uri.URI
  { Uri.uriScheme = Text.unpack (protocol dsn) <> ":"
  , Uri.uriAuthority = Just Uri.URIAuth
    { Uri.uriUserInfo = Text.unpack (publicKey dsn)
      <> maybe "" (\ x -> ":" <> Text.unpack x) (secretKey dsn) <> "@"
    , Uri.uriRegName = Text.unpack (host dsn)
      <> maybe "" (\ x -> ":" <> Text.unpack x) (port dsn)
    , Uri.uriPort = ""
    }
  , Uri.uriPath = Text.unpack (path dsn) <> Text.unpack (projectId dsn)
  , Uri.uriQuery = ""
  , Uri.uriFragment = ""
  }

toString :: Dsn -> String
toString dsn = Uri.uriToString id (toUri dsn) ""
