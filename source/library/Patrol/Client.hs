-- In the use of ‘intoRequest’ (imported from Patrol.Type.Event)
{-# OPTIONS_GHC -Wno-deprecations #-}

module Patrol.Client where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
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
  captureExceptionWith pure dsn e

captureExceptionWith ::
  (Catch.Exception e, IO.MonadIO io, Catch.MonadThrow io) =>
  -- | How to modify the 'Event.Event' before it is sent. Use @'pure'@ if you
  -- don't want to modify the event.
  (Event.Event -> io Event.Event) ->
  Dsn.Dsn ->
  e ->
  io Response.Response
captureExceptionWith modifyEvent dsn e = do
  initialEvent <- Event.fromSomeException $ Catch.toException e
  event <- modifyEvent initialEvent
  let envelope = Envelope.fromEvent dsn event
  request <- Envelope.intoRequest dsn envelope
  manager <- Tls.newTlsManager
  response <- IO.liftIO $ Client.httpLbs request manager
  either (Catch.throwM . Problem.Problem . mappend "invalid response body: ") pure
    . Aeson.eitherDecode
    $ Client.responseBody response

{-# DEPRECATED store "Use 'captureException' instead." #-}
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
