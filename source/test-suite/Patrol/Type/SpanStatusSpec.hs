{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.SpanStatusSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Patrol.Type.SpanStatus as SpanStatus
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.SpanStatus" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works for Aborted" $ do
      Aeson.toJSON SpanStatus.Aborted `Hspec.shouldBe` [Aeson.aesonQQ| "aborted" |]

    Hspec.it "works for AlreadyExists" $ do
      Aeson.toJSON SpanStatus.AlreadyExists `Hspec.shouldBe` [Aeson.aesonQQ| "already_exists" |]

    Hspec.it "works for Cancelled" $ do
      Aeson.toJSON SpanStatus.Cancelled `Hspec.shouldBe` [Aeson.aesonQQ| "cancelled" |]

    Hspec.it "works for DataLoss" $ do
      Aeson.toJSON SpanStatus.DataLoss `Hspec.shouldBe` [Aeson.aesonQQ| "data_loss" |]

    Hspec.it "works for DeadlineExceeded" $ do
      Aeson.toJSON SpanStatus.DeadlineExceeded `Hspec.shouldBe` [Aeson.aesonQQ| "deadline_exceeded" |]

    Hspec.it "works for FailedPrecondition" $ do
      Aeson.toJSON SpanStatus.FailedPrecondition `Hspec.shouldBe` [Aeson.aesonQQ| "failed_precondition" |]

    Hspec.it "works for InternalError" $ do
      Aeson.toJSON SpanStatus.InternalError `Hspec.shouldBe` [Aeson.aesonQQ| "internal_error" |]

    Hspec.it "works for InvalidArgument" $ do
      Aeson.toJSON SpanStatus.InvalidArgument `Hspec.shouldBe` [Aeson.aesonQQ| "invalid_argument" |]

    Hspec.it "works for NotFound" $ do
      Aeson.toJSON SpanStatus.NotFound `Hspec.shouldBe` [Aeson.aesonQQ| "not_found" |]

    Hspec.it "works for Ok" $ do
      Aeson.toJSON SpanStatus.Ok `Hspec.shouldBe` [Aeson.aesonQQ| "ok" |]

    Hspec.it "works for OutOfRange" $ do
      Aeson.toJSON SpanStatus.OutOfRange `Hspec.shouldBe` [Aeson.aesonQQ| "out_of_range" |]

    Hspec.it "works for PermissionDenied" $ do
      Aeson.toJSON SpanStatus.PermissionDenied `Hspec.shouldBe` [Aeson.aesonQQ| "permission_denied" |]

    Hspec.it "works for ResourceExhausted" $ do
      Aeson.toJSON SpanStatus.ResourceExhausted `Hspec.shouldBe` [Aeson.aesonQQ| "resource_exhausted" |]

    Hspec.it "works for Unauthenticated" $ do
      Aeson.toJSON SpanStatus.Unauthenticated `Hspec.shouldBe` [Aeson.aesonQQ| "unauthenticated" |]

    Hspec.it "works for Unavailable" $ do
      Aeson.toJSON SpanStatus.Unavailable `Hspec.shouldBe` [Aeson.aesonQQ| "unavailable" |]

    Hspec.it "works for Unimplemented" $ do
      Aeson.toJSON SpanStatus.Unimplemented `Hspec.shouldBe` [Aeson.aesonQQ| "unimplemented" |]

    Hspec.it "works for Unknown" $ do
      Aeson.toJSON SpanStatus.Unknown `Hspec.shouldBe` [Aeson.aesonQQ| "unknown" |]
