# Patrol

[![CI](https://github.com/tfausak/patrol/actions/workflows/ci.yml/badge.svg)](https://github.com/tfausak/patrol/actions/workflows/ci.yml)
[![Hackage](https://badgen.net/hackage/v/patrol)](https://hackage.haskell.org/package/patrol)

Patrol is a Sentry SDK for Haskell.

## Usage

### Basic exception reporting

The simplest way to report an exception is with `captureException`. It reads the
DSN from the `SENTRY_DSN` environment variable.

```haskell
import qualified Patrol

main :: IO ()
main = do
  result <- doSomething
  case result of
    Left err -> do
      _ <- Patrol.captureException err
      pure ()
    Right val -> use val
```

### Customizing the event

Use `captureExceptionWith` to add tags, set the environment, attach user info, or
otherwise modify the event before it is sent.

```haskell
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.Event as Event
import qualified Patrol.Type.Level as Level
import qualified Patrol.Type.User as User

reportException :: Dsn.Dsn -> SomeException -> IO ()
reportException dsn err = do
  _ <- Patrol.captureExceptionWith modifyEvent dsn err
  pure ()
  where
    modifyEvent :: Event.Event -> IO Event.Event
    modifyEvent event = pure event
      { Event.tags = Map.fromList
          [ (Text.pack "component", Text.pack "api")
          ]
      , Event.level = Just Level.Warning
      , Event.user = Just User.empty
          { User.id = Text.pack "user-123"
          , User.email = Text.pack "user@example.com"
          }
      , Event.environment = Text.pack "staging"
      }
```

### Warp middleware

Here is an example of a [Warp](https://hackage.haskell.org/package/warp)
middleware that reports non-success HTTP responses to Sentry.

```haskell
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Patrol
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.Event as Event

sentryMiddleware :: Dsn.Dsn -> Wai.Middleware
sentryMiddleware dsn app request respond =
  app request $ \response -> do
    let status = Wai.responseStatus response
    Control.Monad.when (Http.statusCode status >= 500) $ do
      let err = userError $ "HTTP " <> show (Http.statusCode status)
      _ <- Patrol.captureExceptionWith (modifyEvent request status) dsn err
      pure ()
    respond response
  where
    modifyEvent :: Wai.Request -> Http.Status -> Event.Event -> IO Event.Event
    modifyEvent request status event = pure event
      { Event.tags = Map.fromList
          [ (Text.pack "path", Text.decodeUtf8 (Wai.rawPathInfo request))
          , (Text.pack "status_code", Text.pack (show (Http.statusCode status)))
          ]
      }
```
