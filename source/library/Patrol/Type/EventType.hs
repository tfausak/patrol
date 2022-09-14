module Patrol.Type.EventType where

import qualified Data.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#eventtype>
data EventType
  = Csp
  | Default
  | Error
  | Expectct
  | Expectstaple
  | Hpkp
  | Transaction
  deriving (Eq, Show)

instance Aeson.ToJSON EventType where
  toJSON eventType = Aeson.toJSON $ case eventType of
    Csp -> "csp"
    Default -> "default"
    Error -> "error"
    Expectct -> "expectct"
    Expectstaple -> "expectstaple"
    Hpkp -> "hpkp"
    Transaction -> "transaction"
