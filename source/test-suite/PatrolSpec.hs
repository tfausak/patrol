module PatrolSpec where

import qualified Patrol.ClientSpec
import qualified Patrol.ConstantSpec
import qualified Patrol.Extra.AesonSpec
import qualified Patrol.Extra.ListSpec
import qualified Patrol.Type.AppContextSpec
import qualified Patrol.Type.AppleDebugImageSpec
import qualified Patrol.Type.BreadcrumbSpec
import qualified Patrol.Type.BreadcrumbTypeSpec
import qualified Patrol.Type.BreadcrumbsSpec
import qualified Patrol.Type.BrowserContextSpec
import qualified Patrol.Type.CErrorSpec
import qualified Patrol.Type.ClientSdkInfoSpec
import qualified Patrol.Type.ClientSdkPackageSpec
import qualified Patrol.Type.ContextSpec
import qualified Patrol.Type.DebugImageSpec
import qualified Patrol.Type.DebugMetaSpec
import qualified Patrol.Type.DeviceContextSpec
import qualified Patrol.Type.DsnSpec
import qualified Patrol.Type.EnvelopeSpec
import qualified Patrol.Type.ErrorTypeSpec
import qualified Patrol.Type.EventIdSpec
import qualified Patrol.Type.EventProcessingErrorSpec
import qualified Patrol.Type.EventSpec
import qualified Patrol.Type.EventTypeSpec
import qualified Patrol.Type.ExceptionSpec
import qualified Patrol.Type.ExceptionsSpec
import qualified Patrol.Type.FrameSpec
import qualified Patrol.Type.GeoSpec
import qualified Patrol.Type.GpuContextSpec
import qualified Patrol.Type.HeadersSpec
import qualified Patrol.Type.ItemSpec
import qualified Patrol.Type.ItemsSpec
import qualified Patrol.Type.LevelSpec
import qualified Patrol.Type.LogEntrySpec
import qualified Patrol.Type.MachExceptionSpec
import qualified Patrol.Type.MechanismMetaSpec
import qualified Patrol.Type.MechanismSpec
import qualified Patrol.Type.NativeDebugImageSpec
import qualified Patrol.Type.NsErrorSpec
import qualified Patrol.Type.OsContextSpec
import qualified Patrol.Type.PlatformSpec
import qualified Patrol.Type.PosixSignalSpec
import qualified Patrol.Type.ProguardDebugImageSpec
import qualified Patrol.Type.RequestSpec
import qualified Patrol.Type.ResponseSpec
import qualified Patrol.Type.RuntimeContextSpec
import qualified Patrol.Type.SpanStatusSpec
import qualified Patrol.Type.StacktraceSpec
import qualified Patrol.Type.SystemSdkInfoSpec
import qualified Patrol.Type.ThreadSpec
import qualified Patrol.Type.ThreadsSpec
import qualified Patrol.Type.TraceContextSpec
import qualified Patrol.Type.TransactionInfoSpec
import qualified Patrol.Type.TransactionSourceSpec
import qualified Patrol.Type.UserSpec
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do
  Patrol.ClientSpec.spec
  Patrol.ConstantSpec.spec
  Patrol.Extra.AesonSpec.spec
  Patrol.Extra.ListSpec.spec
  Patrol.Type.AppContextSpec.spec
  Patrol.Type.AppleDebugImageSpec.spec
  Patrol.Type.BreadcrumbSpec.spec
  Patrol.Type.BreadcrumbsSpec.spec
  Patrol.Type.BreadcrumbTypeSpec.spec
  Patrol.Type.BrowserContextSpec.spec
  Patrol.Type.CErrorSpec.spec
  Patrol.Type.ClientSdkInfoSpec.spec
  Patrol.Type.ClientSdkPackageSpec.spec
  Patrol.Type.ContextSpec.spec
  Patrol.Type.DebugImageSpec.spec
  Patrol.Type.DebugMetaSpec.spec
  Patrol.Type.DeviceContextSpec.spec
  Patrol.Type.DsnSpec.spec
  Patrol.Type.EnvelopeSpec.spec
  Patrol.Type.HeadersSpec.spec
  Patrol.Type.ItemSpec.spec
  Patrol.Type.ItemsSpec.spec
  Patrol.Type.ErrorTypeSpec.spec
  Patrol.Type.EventIdSpec.spec
  Patrol.Type.EventProcessingErrorSpec.spec
  Patrol.Type.EventSpec.spec
  Patrol.Type.EventTypeSpec.spec
  Patrol.Type.ExceptionSpec.spec
  Patrol.Type.ExceptionsSpec.spec
  Patrol.Type.FrameSpec.spec
  Patrol.Type.GeoSpec.spec
  Patrol.Type.GpuContextSpec.spec
  Patrol.Type.LevelSpec.spec
  Patrol.Type.LogEntrySpec.spec
  Patrol.Type.MachExceptionSpec.spec
  Patrol.Type.MechanismMetaSpec.spec
  Patrol.Type.MechanismSpec.spec
  Patrol.Type.NativeDebugImageSpec.spec
  Patrol.Type.NsErrorSpec.spec
  Patrol.Type.OsContextSpec.spec
  Patrol.Type.PlatformSpec.spec
  Patrol.Type.PosixSignalSpec.spec
  Patrol.Type.ProguardDebugImageSpec.spec
  Patrol.Type.RequestSpec.spec
  Patrol.Type.ResponseSpec.spec
  Patrol.Type.RuntimeContextSpec.spec
  Patrol.Type.SpanStatusSpec.spec
  Patrol.Type.StacktraceSpec.spec
  Patrol.Type.SystemSdkInfoSpec.spec
  Patrol.Type.ThreadSpec.spec
  Patrol.Type.ThreadsSpec.spec
  Patrol.Type.TraceContextSpec.spec
  Patrol.Type.TransactionInfoSpec.spec
  Patrol.Type.TransactionSourceSpec.spec
  Patrol.Type.UserSpec.spec
