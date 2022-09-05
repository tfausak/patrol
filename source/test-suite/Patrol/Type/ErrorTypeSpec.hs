module Patrol.Type.ErrorTypeSpec where

import qualified Data.Aeson as Aeson
import qualified Patrol.Type.ErrorType as ErrorType
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.ErrorType" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      Aeson.encode ErrorType.ClockDrift `Hspec.shouldBe` Aeson.encode "clock_drift"
      Aeson.encode ErrorType.FetchGenericError `Hspec.shouldBe` Aeson.encode "fetch_generic_error"
      Aeson.encode ErrorType.FetchInvalidEncoding `Hspec.shouldBe` Aeson.encode "fetch_invalid_source_encoding"
      Aeson.encode ErrorType.FetchInvalidHttpCode `Hspec.shouldBe` Aeson.encode "fetch_invalid_http_code"
      Aeson.encode ErrorType.FetchTimeout `Hspec.shouldBe` Aeson.encode "fetch_timeout"
      Aeson.encode ErrorType.FetchTooLarge `Hspec.shouldBe` Aeson.encode "fetch_too_large"
      Aeson.encode ErrorType.FutureTimestamp `Hspec.shouldBe` Aeson.encode "future_timestamp"
      Aeson.encode ErrorType.InvalidAttribute `Hspec.shouldBe` Aeson.encode "invalid_attribute"
      Aeson.encode ErrorType.InvalidData `Hspec.shouldBe` Aeson.encode "invalid_data"
      Aeson.encode ErrorType.InvalidEnvironment `Hspec.shouldBe` Aeson.encode "invalid_environment"
      Aeson.encode ErrorType.JsFetchTimeout `Hspec.shouldBe` Aeson.encode "js_fetch_timeout"
      Aeson.encode ErrorType.JsGenericFetchError `Hspec.shouldBe` Aeson.encode "js_generic_fetch_error"
      Aeson.encode ErrorType.JsInvalidContent `Hspec.shouldBe` Aeson.encode "js_invalid_content"
      Aeson.encode ErrorType.JsInvalidHttpCode `Hspec.shouldBe` Aeson.encode "js_invalid_http_code"
      Aeson.encode ErrorType.JsInvalidSourceEncoding `Hspec.shouldBe` Aeson.encode "js_invalid_source_encoding"
      Aeson.encode ErrorType.JsInvalidSourcemap `Hspec.shouldBe` Aeson.encode "js_invalid_source"
      Aeson.encode ErrorType.JsInvalidSourcemapLocation `Hspec.shouldBe` Aeson.encode "js_invalid_sourcemap_location"
      Aeson.encode ErrorType.JsMissingSource `Hspec.shouldBe` Aeson.encode "js_no_source"
      Aeson.encode ErrorType.JsNoColumn `Hspec.shouldBe` Aeson.encode "js_no_column"
      Aeson.encode ErrorType.JsTooLarge `Hspec.shouldBe` Aeson.encode "js_too_large"
      Aeson.encode ErrorType.JsTooManyRemoteSources `Hspec.shouldBe` Aeson.encode "js_too_many_sources"
      Aeson.encode ErrorType.MissingAttribute `Hspec.shouldBe` Aeson.encode "missing_attribute"
      Aeson.encode ErrorType.NativeBadDsym `Hspec.shouldBe` Aeson.encode "native_bad_dsym"
      Aeson.encode ErrorType.NativeInternalFailure `Hspec.shouldBe` Aeson.encode "native_internal_failure"
      Aeson.encode ErrorType.NativeMissingDsym `Hspec.shouldBe` Aeson.encode "native_missing_dsym"
      Aeson.encode ErrorType.NativeMissingOptionallyBundledDsym `Hspec.shouldBe` Aeson.encode "native_optionally_bundled_dsym"
      Aeson.encode ErrorType.NativeMissingSymbol `Hspec.shouldBe` Aeson.encode "native_missing_symbol"
      Aeson.encode ErrorType.NativeMissingSystemDsym `Hspec.shouldBe` Aeson.encode "native_missing_system_dsym"
      Aeson.encode ErrorType.NativeNoCrashedThread `Hspec.shouldBe` Aeson.encode "native_no_crashed_thread"
      Aeson.encode ErrorType.NativeSimulatorFrame `Hspec.shouldBe` Aeson.encode "native_simulator_frame"
      Aeson.encode ErrorType.NativeSymbolicatorFailed `Hspec.shouldBe` Aeson.encode "native_symbolicator_failed"
      Aeson.encode ErrorType.NativeUnknownImage `Hspec.shouldBe` Aeson.encode "native_unknown_image"
      Aeson.encode ErrorType.PastTimestamp `Hspec.shouldBe` Aeson.encode "past_timestamp"
      Aeson.encode ErrorType.ProguardMissingLineno `Hspec.shouldBe` Aeson.encode "proguard_missing_lineno"
      Aeson.encode ErrorType.ProguardMissingMapping `Hspec.shouldBe` Aeson.encode "proguard_missing_mapping"
      Aeson.encode ErrorType.RestrictedIp `Hspec.shouldBe` Aeson.encode "restricted_ip"
      Aeson.encode ErrorType.SecurityViolation `Hspec.shouldBe` Aeson.encode "security_violation"
      Aeson.encode ErrorType.TooLargeForCache `Hspec.shouldBe` Aeson.encode "too_large_for_cache"
      Aeson.encode ErrorType.UnknownError `Hspec.shouldBe` Aeson.encode "unknown_error"
      Aeson.encode ErrorType.ValueTooLong `Hspec.shouldBe` Aeson.encode "value_too_long"
