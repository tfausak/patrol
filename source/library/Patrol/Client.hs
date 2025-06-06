{-# OPTIONS_GHC -Wno-deprecations #-}

module Patrol.Client where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified GHC.Stack as Stack
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Patrol.Exception.Problem as Problem
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.Envelope as Envelope
import qualified Patrol.Type.Event as Event
import qualified Patrol.Type.Response as Response
import qualified System.Environment as Environment

-- | Capture an exception by sending it to Sentry. The DSN is read from the
-- @SENTRY_DSN@ environment variable. To customize the behavior, use
-- 'captureExceptionWith'.
captureException ::
  (Catch.Exception e, IO.MonadIO io, Catch.MonadThrow io) =>
  e ->
  io Response.Response
captureException e = do
  dsn <- do
    maybeString <- IO.liftIO $ Environment.lookupEnv "SENTRY_DSN"
    Dsn.fromText $ maybe Text.empty Text.pack maybeString
  captureExceptionWith (const Nothing) pure dsn e

captureExceptionWith ::
  (Catch.Exception e, IO.MonadIO io, Catch.MonadThrow io) =>
  -- | How to get a 'Stack.CallStack' from a 'Catch.SomeException'. Use
  -- @'const' 'Nothing'@ if you don't want to get a call stack.
  (Catch.SomeException -> Maybe Stack.CallStack) ->
  -- | How to modify the 'Envelope.Envelope' before it is sent. Use @'pure'@ if
  -- you don't want to modify the envelope.
  (Envelope.Envelope -> io Envelope.Envelope) ->
  Dsn.Dsn ->
  e ->
  io Response.Response
captureExceptionWith getCallStack modifyEnvelope dsn e = do
  initialEnvelope <- Envelope.fromException getCallStack dsn e
  envelope <- modifyEnvelope initialEnvelope
  request <- Envelope.intoRequest dsn envelope
  manager <- Tls.newTlsManager
  response <- IO.liftIO $ Client.httpLbs request manager
  either (Catch.throwM . Problem.Problem . mappend "invalid response body: ") pure
    . Aeson.eitherDecode
    $ Client.responseBody response

{-# DEPRECATED store "Use `captureException` instead." #-}
store ::
  (IO.MonadIO io, Catch.MonadThrow io) =>
  Client.Manager ->
  Dsn.Dsn ->
  Event.Event ->
  io Response.Response
store manager dsn event = do
  request <- Event.intoRequest dsn event
  response <- IO.liftIO $ Client.httpLbs request manager
  either (Catch.throwM . Problem.Problem . mappend "invalid response body: ") pure
    . Aeson.eitherDecode
    $ Client.responseBody response
