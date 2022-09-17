{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ErrorTypeSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Patrol.Type.ErrorType as ErrorType
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.ErrorType" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works for ClockDrift" $ do
      Aeson.toJSON ErrorType.ClockDrift `Hspec.shouldBe` [Aeson.aesonQQ| "clock_drift" |]

    Hspec.it "works for FetchGenericError" $ do
      Aeson.toJSON ErrorType.FetchGenericError `Hspec.shouldBe` [Aeson.aesonQQ| "fetch_generic_error" |]

    Hspec.it "works for FetchInvalidEncoding" $ do
      Aeson.toJSON ErrorType.FetchInvalidEncoding `Hspec.shouldBe` [Aeson.aesonQQ| "fetch_invalid_source_encoding" |]

    Hspec.it "works for FetchInvalidHttpCode" $ do
      Aeson.toJSON ErrorType.FetchInvalidHttpCode `Hspec.shouldBe` [Aeson.aesonQQ| "fetch_invalid_http_code" |]

    Hspec.it "works for FetchTimeout" $ do
      Aeson.toJSON ErrorType.FetchTimeout `Hspec.shouldBe` [Aeson.aesonQQ| "fetch_timeout" |]

    Hspec.it "works for FetchTooLarge" $ do
      Aeson.toJSON ErrorType.FetchTooLarge `Hspec.shouldBe` [Aeson.aesonQQ| "fetch_too_large" |]

    Hspec.it "works for FutureTimestamp" $ do
      Aeson.toJSON ErrorType.FutureTimestamp `Hspec.shouldBe` [Aeson.aesonQQ| "future_timestamp" |]

    Hspec.it "works for InvalidAttribute" $ do
      Aeson.toJSON ErrorType.InvalidAttribute `Hspec.shouldBe` [Aeson.aesonQQ| "invalid_attribute" |]

    Hspec.it "works for InvalidData" $ do
      Aeson.toJSON ErrorType.InvalidData `Hspec.shouldBe` [Aeson.aesonQQ| "invalid_data" |]

    Hspec.it "works for InvalidEnvironment" $ do
      Aeson.toJSON ErrorType.InvalidEnvironment `Hspec.shouldBe` [Aeson.aesonQQ| "invalid_environment" |]

    Hspec.it "works for JsFetchTimeout" $ do
      Aeson.toJSON ErrorType.JsFetchTimeout `Hspec.shouldBe` [Aeson.aesonQQ| "js_fetch_timeout" |]

    Hspec.it "works for JsGenericFetchError" $ do
      Aeson.toJSON ErrorType.JsGenericFetchError `Hspec.shouldBe` [Aeson.aesonQQ| "js_generic_fetch_error" |]

    Hspec.it "works for JsInvalidContent" $ do
      Aeson.toJSON ErrorType.JsInvalidContent `Hspec.shouldBe` [Aeson.aesonQQ| "js_invalid_content" |]

    Hspec.it "works for JsInvalidHttpCode" $ do
      Aeson.toJSON ErrorType.JsInvalidHttpCode `Hspec.shouldBe` [Aeson.aesonQQ| "js_invalid_http_code" |]

    Hspec.it "works for JsInvalidSourceEncoding" $ do
      Aeson.toJSON ErrorType.JsInvalidSourceEncoding `Hspec.shouldBe` [Aeson.aesonQQ| "js_invalid_source_encoding" |]

    Hspec.it "works for JsInvalidSourcemap" $ do
      Aeson.toJSON ErrorType.JsInvalidSourcemap `Hspec.shouldBe` [Aeson.aesonQQ| "js_invalid_source" |]

    Hspec.it "works for JsInvalidSourcemapLocation" $ do
      Aeson.toJSON ErrorType.JsInvalidSourcemapLocation `Hspec.shouldBe` [Aeson.aesonQQ| "js_invalid_sourcemap_location" |]

    Hspec.it "works for JsMissingSource" $ do
      Aeson.toJSON ErrorType.JsMissingSource `Hspec.shouldBe` [Aeson.aesonQQ| "js_no_source" |]

    Hspec.it "works for JsNoColumn" $ do
      Aeson.toJSON ErrorType.JsNoColumn `Hspec.shouldBe` [Aeson.aesonQQ| "js_no_column" |]

    Hspec.it "works for JsTooLarge" $ do
      Aeson.toJSON ErrorType.JsTooLarge `Hspec.shouldBe` [Aeson.aesonQQ| "js_too_large" |]

    Hspec.it "works for JsTooManyRemoteSources" $ do
      Aeson.toJSON ErrorType.JsTooManyRemoteSources `Hspec.shouldBe` [Aeson.aesonQQ| "js_too_many_sources" |]

    Hspec.it "works for MissingAttribute" $ do
      Aeson.toJSON ErrorType.MissingAttribute `Hspec.shouldBe` [Aeson.aesonQQ| "missing_attribute" |]

    Hspec.it "works for NativeBadDsym" $ do
      Aeson.toJSON ErrorType.NativeBadDsym `Hspec.shouldBe` [Aeson.aesonQQ| "native_bad_dsym" |]

    Hspec.it "works for NativeInternalFailure" $ do
      Aeson.toJSON ErrorType.NativeInternalFailure `Hspec.shouldBe` [Aeson.aesonQQ| "native_internal_failure" |]

    Hspec.it "works for NativeMissingDsym" $ do
      Aeson.toJSON ErrorType.NativeMissingDsym `Hspec.shouldBe` [Aeson.aesonQQ| "native_missing_dsym" |]

    Hspec.it "works for NativeMissingOptionallyBundledDsym" $ do
      Aeson.toJSON ErrorType.NativeMissingOptionallyBundledDsym `Hspec.shouldBe` [Aeson.aesonQQ| "native_optionally_bundled_dsym" |]

    Hspec.it "works for NativeMissingSymbol" $ do
      Aeson.toJSON ErrorType.NativeMissingSymbol `Hspec.shouldBe` [Aeson.aesonQQ| "native_missing_symbol" |]

    Hspec.it "works for NativeMissingSystemDsym" $ do
      Aeson.toJSON ErrorType.NativeMissingSystemDsym `Hspec.shouldBe` [Aeson.aesonQQ| "native_missing_system_dsym" |]

    Hspec.it "works for NativeNoCrashedThread" $ do
      Aeson.toJSON ErrorType.NativeNoCrashedThread `Hspec.shouldBe` [Aeson.aesonQQ| "native_no_crashed_thread" |]

    Hspec.it "works for NativeSimulatorFrame" $ do
      Aeson.toJSON ErrorType.NativeSimulatorFrame `Hspec.shouldBe` [Aeson.aesonQQ| "native_simulator_frame" |]

    Hspec.it "works for NativeSymbolicatorFailed" $ do
      Aeson.toJSON ErrorType.NativeSymbolicatorFailed `Hspec.shouldBe` [Aeson.aesonQQ| "native_symbolicator_failed" |]

    Hspec.it "works for NativeUnknownImage" $ do
      Aeson.toJSON ErrorType.NativeUnknownImage `Hspec.shouldBe` [Aeson.aesonQQ| "native_unknown_image" |]

    Hspec.it "works for PastTimestamp" $ do
      Aeson.toJSON ErrorType.PastTimestamp `Hspec.shouldBe` [Aeson.aesonQQ| "past_timestamp" |]

    Hspec.it "works for ProguardMissingLineno" $ do
      Aeson.toJSON ErrorType.ProguardMissingLineno `Hspec.shouldBe` [Aeson.aesonQQ| "proguard_missing_lineno" |]

    Hspec.it "works for ProguardMissingMapping" $ do
      Aeson.toJSON ErrorType.ProguardMissingMapping `Hspec.shouldBe` [Aeson.aesonQQ| "proguard_missing_mapping" |]

    Hspec.it "works for RestrictedIp" $ do
      Aeson.toJSON ErrorType.RestrictedIp `Hspec.shouldBe` [Aeson.aesonQQ| "restricted_ip" |]

    Hspec.it "works for SecurityViolation" $ do
      Aeson.toJSON ErrorType.SecurityViolation `Hspec.shouldBe` [Aeson.aesonQQ| "security_violation" |]

    Hspec.it "works for TooLargeForCache" $ do
      Aeson.toJSON ErrorType.TooLargeForCache `Hspec.shouldBe` [Aeson.aesonQQ| "too_large_for_cache" |]

    Hspec.it "works for UnknownError" $ do
      Aeson.toJSON ErrorType.UnknownError `Hspec.shouldBe` [Aeson.aesonQQ| "unknown_error" |]

    Hspec.it "works for ValueTooLong" $ do
      Aeson.toJSON ErrorType.ValueTooLong `Hspec.shouldBe` [Aeson.aesonQQ| "value_too_long" |]
