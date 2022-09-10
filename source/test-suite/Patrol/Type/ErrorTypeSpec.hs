{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ErrorTypeSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Patrol.Type.ErrorType as ErrorType
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.ErrorType" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      Aeson.toJSON ErrorType.ClockDrift `Hspec.shouldBe` [Aeson.aesonQQ| "clock_drift" |]
      Aeson.toJSON ErrorType.FetchGenericError `Hspec.shouldBe` [Aeson.aesonQQ| "fetch_generic_error" |]
      Aeson.toJSON ErrorType.FetchInvalidEncoding `Hspec.shouldBe` [Aeson.aesonQQ| "fetch_invalid_source_encoding" |]
      Aeson.toJSON ErrorType.FetchInvalidHttpCode `Hspec.shouldBe` [Aeson.aesonQQ| "fetch_invalid_http_code" |]
      Aeson.toJSON ErrorType.FetchTimeout `Hspec.shouldBe` [Aeson.aesonQQ| "fetch_timeout" |]
      Aeson.toJSON ErrorType.FetchTooLarge `Hspec.shouldBe` [Aeson.aesonQQ| "fetch_too_large" |]
      Aeson.toJSON ErrorType.FutureTimestamp `Hspec.shouldBe` [Aeson.aesonQQ| "future_timestamp" |]
      Aeson.toJSON ErrorType.InvalidAttribute `Hspec.shouldBe` [Aeson.aesonQQ| "invalid_attribute" |]
      Aeson.toJSON ErrorType.InvalidData `Hspec.shouldBe` [Aeson.aesonQQ| "invalid_data" |]
      Aeson.toJSON ErrorType.InvalidEnvironment `Hspec.shouldBe` [Aeson.aesonQQ| "invalid_environment" |]
      Aeson.toJSON ErrorType.JsFetchTimeout `Hspec.shouldBe` [Aeson.aesonQQ| "js_fetch_timeout" |]
      Aeson.toJSON ErrorType.JsGenericFetchError `Hspec.shouldBe` [Aeson.aesonQQ| "js_generic_fetch_error" |]
      Aeson.toJSON ErrorType.JsInvalidContent `Hspec.shouldBe` [Aeson.aesonQQ| "js_invalid_content" |]
      Aeson.toJSON ErrorType.JsInvalidHttpCode `Hspec.shouldBe` [Aeson.aesonQQ| "js_invalid_http_code" |]
      Aeson.toJSON ErrorType.JsInvalidSourceEncoding `Hspec.shouldBe` [Aeson.aesonQQ| "js_invalid_source_encoding" |]
      Aeson.toJSON ErrorType.JsInvalidSourcemap `Hspec.shouldBe` [Aeson.aesonQQ| "js_invalid_source" |]
      Aeson.toJSON ErrorType.JsInvalidSourcemapLocation `Hspec.shouldBe` [Aeson.aesonQQ| "js_invalid_sourcemap_location" |]
      Aeson.toJSON ErrorType.JsMissingSource `Hspec.shouldBe` [Aeson.aesonQQ| "js_no_source" |]
      Aeson.toJSON ErrorType.JsNoColumn `Hspec.shouldBe` [Aeson.aesonQQ| "js_no_column" |]
      Aeson.toJSON ErrorType.JsTooLarge `Hspec.shouldBe` [Aeson.aesonQQ| "js_too_large" |]
      Aeson.toJSON ErrorType.JsTooManyRemoteSources `Hspec.shouldBe` [Aeson.aesonQQ| "js_too_many_sources" |]
      Aeson.toJSON ErrorType.MissingAttribute `Hspec.shouldBe` [Aeson.aesonQQ| "missing_attribute" |]
      Aeson.toJSON ErrorType.NativeBadDsym `Hspec.shouldBe` [Aeson.aesonQQ| "native_bad_dsym" |]
      Aeson.toJSON ErrorType.NativeInternalFailure `Hspec.shouldBe` [Aeson.aesonQQ| "native_internal_failure" |]
      Aeson.toJSON ErrorType.NativeMissingDsym `Hspec.shouldBe` [Aeson.aesonQQ| "native_missing_dsym" |]
      Aeson.toJSON ErrorType.NativeMissingOptionallyBundledDsym `Hspec.shouldBe` [Aeson.aesonQQ| "native_optionally_bundled_dsym" |]
      Aeson.toJSON ErrorType.NativeMissingSymbol `Hspec.shouldBe` [Aeson.aesonQQ| "native_missing_symbol" |]
      Aeson.toJSON ErrorType.NativeMissingSystemDsym `Hspec.shouldBe` [Aeson.aesonQQ| "native_missing_system_dsym" |]
      Aeson.toJSON ErrorType.NativeNoCrashedThread `Hspec.shouldBe` [Aeson.aesonQQ| "native_no_crashed_thread" |]
      Aeson.toJSON ErrorType.NativeSimulatorFrame `Hspec.shouldBe` [Aeson.aesonQQ| "native_simulator_frame" |]
      Aeson.toJSON ErrorType.NativeSymbolicatorFailed `Hspec.shouldBe` [Aeson.aesonQQ| "native_symbolicator_failed" |]
      Aeson.toJSON ErrorType.NativeUnknownImage `Hspec.shouldBe` [Aeson.aesonQQ| "native_unknown_image" |]
      Aeson.toJSON ErrorType.PastTimestamp `Hspec.shouldBe` [Aeson.aesonQQ| "past_timestamp" |]
      Aeson.toJSON ErrorType.ProguardMissingLineno `Hspec.shouldBe` [Aeson.aesonQQ| "proguard_missing_lineno" |]
      Aeson.toJSON ErrorType.ProguardMissingMapping `Hspec.shouldBe` [Aeson.aesonQQ| "proguard_missing_mapping" |]
      Aeson.toJSON ErrorType.RestrictedIp `Hspec.shouldBe` [Aeson.aesonQQ| "restricted_ip" |]
      Aeson.toJSON ErrorType.SecurityViolation `Hspec.shouldBe` [Aeson.aesonQQ| "security_violation" |]
      Aeson.toJSON ErrorType.TooLargeForCache `Hspec.shouldBe` [Aeson.aesonQQ| "too_large_for_cache" |]
      Aeson.toJSON ErrorType.UnknownError `Hspec.shouldBe` [Aeson.aesonQQ| "unknown_error" |]
      Aeson.toJSON ErrorType.ValueTooLong `Hspec.shouldBe` [Aeson.aesonQQ| "value_too_long" |]
