module Patrol.Constant where

import qualified Data.ByteString as ByteString
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Version as Version
import qualified Paths_patrol as Package

applicationJson :: ByteString.ByteString
applicationJson = Text.encodeUtf8 $ Text.pack "application/json"

sentryVersion :: Text.Text
sentryVersion = Text.singleton '7'

userAgent :: ByteString.ByteString
userAgent = Text.encodeUtf8 . Text.pack $ "patrol/" <> Version.showVersion Package.version

xSentryAuth :: CI.CI ByteString.ByteString
xSentryAuth = CI.mk . Text.encodeUtf8 $ Text.pack "X-Sentry-Auth"
