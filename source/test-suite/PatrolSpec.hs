module PatrolSpec where

import qualified Patrol.ClientSpec
import qualified Patrol.ConstantSpec
import qualified Patrol.Extra.AesonSpec
import qualified Patrol.Extra.ListSpec
import qualified Patrol.Type.CErrorSpec
import qualified Patrol.Type.DsnSpec
import qualified Patrol.Type.ErrorTypeSpec
import qualified Patrol.Type.EventIdSpec
import qualified Patrol.Type.EventProcessingErrorSpec
import qualified Patrol.Type.EventSpec
import qualified Patrol.Type.ExceptionValueSpec
import qualified Patrol.Type.ExceptionsSpec
import qualified Patrol.Type.FrameSpec
import qualified Patrol.Type.LevelSpec
import qualified Patrol.Type.MachExceptionSpec
import qualified Patrol.Type.MechanismMetaSpec
import qualified Patrol.Type.MechanismSpec
import qualified Patrol.Type.NsErrorSpec
import qualified Patrol.Type.PlatformSpec
import qualified Patrol.Type.PosixSignalSpec
import qualified Patrol.Type.ResponseSpec
import qualified Patrol.Type.StacktraceSpec
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do
  Patrol.ClientSpec.spec
  Patrol.ConstantSpec.spec
  Patrol.Extra.AesonSpec.spec
  Patrol.Extra.ListSpec.spec
  Patrol.Type.CErrorSpec.spec
  Patrol.Type.DsnSpec.spec
  Patrol.Type.ErrorTypeSpec.spec
  Patrol.Type.EventIdSpec.spec
  Patrol.Type.EventProcessingErrorSpec.spec
  Patrol.Type.EventSpec.spec
  Patrol.Type.ExceptionsSpec.spec
  Patrol.Type.ExceptionValueSpec.spec
  Patrol.Type.FrameSpec.spec
  Patrol.Type.LevelSpec.spec
  Patrol.Type.MachExceptionSpec.spec
  Patrol.Type.MechanismMetaSpec.spec
  Patrol.Type.MechanismSpec.spec
  Patrol.Type.NsErrorSpec.spec
  Patrol.Type.PlatformSpec.spec
  Patrol.Type.PosixSignalSpec.spec
  Patrol.Type.ResponseSpec.spec
  Patrol.Type.StacktraceSpec.spec
