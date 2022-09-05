module Patrol.Type.ErrorType where

import qualified Data.Aeson as Aeson

data ErrorType
  = ClockDrift
  | FetchGenericError
  | FetchInvalidEncoding
  | FetchInvalidHttpCode
  | FetchTimeout
  | FetchTooLarge
  | FutureTimestamp
  | InvalidAttribute
  | InvalidData
  | InvalidEnvironment
  | JsFetchTimeout
  | JsGenericFetchError
  | JsInvalidContent
  | JsInvalidHttpCode
  | JsInvalidSourceEncoding
  | JsInvalidSourcemap
  | JsInvalidSourcemapLocation
  | JsMissingSource
  | JsNoColumn
  | JsTooLarge
  | JsTooManyRemoteSources
  | MissingAttribute
  | NativeBadDsym
  | NativeInternalFailure
  | NativeMissingDsym
  | NativeMissingOptionallyBundledDsym
  | NativeMissingSymbol
  | NativeMissingSystemDsym
  | NativeNoCrashedThread
  | NativeSimulatorFrame
  | NativeSymbolicatorFailed
  | NativeUnknownImage
  | PastTimestamp
  | ProguardMissingLineno
  | ProguardMissingMapping
  | RestrictedIp
  | SecurityViolation
  | TooLargeForCache
  | UnknownError
  | ValueTooLong
  deriving (Eq, Show)

instance Aeson.ToJSON ErrorType where
  toJSON errorType = Aeson.toJSON $ case errorType of
    ClockDrift -> "clock_drift"
    FetchGenericError -> "fetch_generic_error"
    FetchInvalidEncoding -> "fetch_invalid_source_encoding"
    FetchInvalidHttpCode -> "fetch_invalid_http_code"
    FetchTimeout -> "fetch_timeout"
    FetchTooLarge -> "fetch_too_large"
    FutureTimestamp -> "future_timestamp"
    InvalidAttribute -> "invalid_attribute"
    InvalidData -> "invalid_data"
    InvalidEnvironment -> "invalid_environment"
    JsFetchTimeout -> "js_fetch_timeout"
    JsGenericFetchError -> "js_generic_fetch_error"
    JsInvalidContent -> "js_invalid_content"
    JsInvalidHttpCode -> "js_invalid_http_code"
    JsInvalidSourceEncoding -> "js_invalid_source_encoding"
    JsInvalidSourcemap -> "js_invalid_source"
    JsInvalidSourcemapLocation -> "js_invalid_sourcemap_location"
    JsMissingSource -> "js_no_source"
    JsNoColumn -> "js_no_column"
    JsTooLarge -> "js_too_large"
    JsTooManyRemoteSources -> "js_too_many_sources"
    MissingAttribute -> "missing_attribute"
    NativeBadDsym -> "native_bad_dsym"
    NativeInternalFailure -> "native_internal_failure"
    NativeMissingDsym -> "native_missing_dsym"
    NativeMissingOptionallyBundledDsym -> "native_optionally_bundled_dsym"
    NativeMissingSymbol -> "native_missing_symbol"
    NativeMissingSystemDsym -> "native_missing_system_dsym"
    NativeNoCrashedThread -> "native_no_crashed_thread"
    NativeSimulatorFrame -> "native_simulator_frame"
    NativeSymbolicatorFailed -> "native_symbolicator_failed"
    NativeUnknownImage -> "native_unknown_image"
    PastTimestamp -> "past_timestamp"
    ProguardMissingLineno -> "proguard_missing_lineno"
    ProguardMissingMapping -> "proguard_missing_mapping"
    RestrictedIp -> "restricted_ip"
    SecurityViolation -> "security_violation"
    TooLargeForCache -> "too_large_for_cache"
    UnknownError -> "unknown_error"
    ValueTooLong -> "value_too_long"
