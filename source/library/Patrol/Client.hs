module Patrol.Client where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Client as Client
import qualified Patrol.Exception.Problem as Problem
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.Event as Event
import qualified Patrol.Type.Response as Response

store ::
  (IO.MonadIO io, Catch.MonadThrow io) =>
  Client.Manager ->
  Dsn.Dsn ->
  Event.Event ->
  io Response.Response
store manager dsn event = do
  request <- Event.intoRequest dsn event
  response <- IO.liftIO $ Client.httpLbs request manager
  maybe (Catch.throwM $ Problem.Problem "invalid response body") pure
    . Aeson.decode
    $ Client.responseBody response
