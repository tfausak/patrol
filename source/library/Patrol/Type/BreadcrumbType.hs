module Patrol.Type.BreadcrumbType where

import qualified Data.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/data-model/event-payloads/breadcrumbs/#breadcrumb-types>
data BreadcrumbType
  = -- | A generic breadcrumb, typically a log message or something user-generated.
    Default
  | -- | A generic debug event, typically a log message.
    Debug
  | -- | An error that occurred before the exception that generates an
    -- 'Patrol.Type.Event.Event'.
    Error
  | -- | A URL change in a web application, a UI transition on mobile or desktop,
    -- or some other event with similar semantics.
    Navigation
  | -- | An HTTP request transmitted from your application.
    Http
  | -- | Information that helps identify the root cause of the issue or for whom
    -- the error occurred.
    Info
  | -- | A query made to the application.
    Query
  | -- | A tracing event.
    Transaction
  | -- | A user interaction with the application's UI, synonymouos with 'User'.
    UI
  | -- | A user interaction with the application's UI, synonymous with 'UI'.
    User
  deriving (Eq, Show)

instance Aeson.ToJSON BreadcrumbType where
  toJSON breadcrumbType = Aeson.toJSON $ case breadcrumbType of
    Default -> "default"
    Debug -> "debug"
    Error -> "error"
    Navigation -> "navigation"
    Http -> "http"
    Info -> "info"
    Query -> "query"
    Transaction -> "transaction"
    UI -> "ui"
    User -> "user"
