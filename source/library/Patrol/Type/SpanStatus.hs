module Patrol.Type.SpanStatus where

import qualified Data.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#spanstatus>
data SpanStatus
  = Aborted
  | AlreadyExists
  | Cancelled
  | DataLoss
  | DeadlineExceeded
  | FailedPrecondition
  | InternalError
  | InvalidArgument
  | NotFound
  | Ok
  | OutOfRange
  | PermissionDenied
  | ResourceExhausted
  | Unauthenticated
  | Unavailable
  | Unimplemented
  | Unknown
  deriving (Eq, Show)

instance Aeson.ToJSON SpanStatus where
  toJSON spanStatus = Aeson.toJSON $ case spanStatus of
    Aborted -> "aborted"
    AlreadyExists -> "already_exists"
    Cancelled -> "cancelled"
    DataLoss -> "data_loss"
    DeadlineExceeded -> "deadline_exceeded"
    FailedPrecondition -> "failed_precondition"
    InternalError -> "internal_error"
    InvalidArgument -> "invalid_argument"
    NotFound -> "not_found"
    Ok -> "ok"
    OutOfRange -> "out_of_range"
    PermissionDenied -> "permission_denied"
    ResourceExhausted -> "resource_exhausted"
    Unauthenticated -> "unauthenticated"
    Unavailable -> "unavailable"
    Unimplemented -> "unimplemented"
    Unknown -> "unknown"
