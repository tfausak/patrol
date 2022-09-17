{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.TraceContextSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.SpanStatus as SpanStatus
import qualified Patrol.Type.TraceContext as TraceContext
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.TraceContext" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let traceContext = TraceContext.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON traceContext `Hspec.shouldBe` json

    Hspec.it "works with exclusiveTime" $ do
      let traceContext = TraceContext.empty {TraceContext.exclusiveTime = Just 0}
          json = [Aeson.aesonQQ| { "exclusive_time": 0 } |]
      Aeson.toJSON traceContext `Hspec.shouldBe` json

    Hspec.it "works with op" $ do
      let traceContext = TraceContext.empty {TraceContext.op = Text.pack "example-op"}
          json = [Aeson.aesonQQ| { "op": "example-op" } |]
      Aeson.toJSON traceContext `Hspec.shouldBe` json

    Hspec.it "works with parentSpanId" $ do
      let traceContext = TraceContext.empty {TraceContext.parentSpanId = Text.pack "example-parentSpanId"}
          json = [Aeson.aesonQQ| { "parent_span_id": "example-parentSpanId" } |]
      Aeson.toJSON traceContext `Hspec.shouldBe` json

    Hspec.it "works with spanId" $ do
      let traceContext = TraceContext.empty {TraceContext.spanId = Text.pack "example-spanId"}
          json = [Aeson.aesonQQ| { "span_id": "example-spanId" } |]
      Aeson.toJSON traceContext `Hspec.shouldBe` json

    Hspec.it "works with status" $ do
      let traceContext = TraceContext.empty {TraceContext.status = Just SpanStatus.Unknown}
          json = [Aeson.aesonQQ| { "status": "unknown" } |]
      Aeson.toJSON traceContext `Hspec.shouldBe` json

    Hspec.it "works with traceId" $ do
      let traceContext = TraceContext.empty {TraceContext.traceId = Text.pack "example-traceId"}
          json = [Aeson.aesonQQ| { "trace_id": "example-traceId" } |]
      Aeson.toJSON traceContext `Hspec.shouldBe` json
