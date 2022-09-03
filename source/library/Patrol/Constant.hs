module Patrol.Constant where

import qualified Data.ByteString as ByteString
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as Http
import qualified Patrol.Version as Version

applicationJson :: ByteString.ByteString
applicationJson = Text.encodeUtf8 $ Text.pack "application/json"

sentryVersion :: Text.Text
sentryVersion = Text.singleton '7'

userAgent :: Text.Text
userAgent = Text.pack "patrol/" <> Version.text

xSentryAuth :: Http.HeaderName
xSentryAuth = CI.mk . Text.encodeUtf8 $ Text.pack "X-Sentry-Auth"
