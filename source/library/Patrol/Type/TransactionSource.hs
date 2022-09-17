module Patrol.Type.TransactionSource where

import qualified Data.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#transactionsource>
data TransactionSource
  = Component
  | Custom
  | Route
  | Task
  | Url
  | Unknown
  | View
  deriving (Eq, Show)

instance Aeson.ToJSON TransactionSource where
  toJSON transactionSource = Aeson.toJSON $ case transactionSource of
    Component -> "component"
    Custom -> "custom"
    Route -> "route"
    Task -> "task"
    Url -> "url"
    Unknown -> "unknown"
    View -> "view"
