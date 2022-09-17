module Patrol.Type.TraceContext where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.SpanStatus as SpanStatus

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#tracecontext>
data TraceContext = TraceContext
  { exclusiveTime :: Maybe Int,
    op :: Text.Text,
    parentSpanId :: Text.Text,
    spanId :: Text.Text,
    status :: Maybe SpanStatus.SpanStatus,
    traceId :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON TraceContext where
  toJSON traceContext =
    Aeson.intoObject
      [ Aeson.pair "exclusive_time" $ exclusiveTime traceContext,
        Aeson.pair "op" $ op traceContext,
        Aeson.pair "parent_span_id" $ parentSpanId traceContext,
        Aeson.pair "span_id" $ spanId traceContext,
        Aeson.pair "status" $ status traceContext,
        Aeson.pair "trace_id" $ traceId traceContext
      ]

empty :: TraceContext
empty =
  TraceContext
    { exclusiveTime = Nothing,
      op = Text.empty,
      parentSpanId = Text.empty,
      spanId = Text.empty,
      status = Nothing,
      traceId = Text.empty
    }
