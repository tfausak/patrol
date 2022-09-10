module PatrolSpec where

import qualified Patrol.ClientSpec
import qualified Patrol.ConstantSpec
import qualified Patrol.Extra.AesonSpec
import qualified Patrol.Extra.ListSpec
import qualified Patrol.Type.DsnSpec
import qualified Patrol.Type.ErrorSpec
import qualified Patrol.Type.ErrorTypeSpec
import qualified Patrol.Type.EventIdSpec
import qualified Patrol.Type.EventSpec
import qualified Patrol.Type.ExceptionSpec
import qualified Patrol.Type.LevelSpec
import qualified Patrol.Type.MechanismSpec
import qualified Patrol.Type.PlatformSpec
import qualified Patrol.Type.ResponseSpec
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do
  Patrol.ClientSpec.spec
  Patrol.ConstantSpec.spec
  Patrol.Extra.AesonSpec.spec
  Patrol.Extra.ListSpec.spec
  Patrol.Type.DsnSpec.spec
  Patrol.Type.ErrorSpec.spec
  Patrol.Type.ErrorTypeSpec.spec
  Patrol.Type.EventIdSpec.spec
  Patrol.Type.EventSpec.spec
  Patrol.Type.ExceptionSpec.spec
  Patrol.Type.LevelSpec.spec
  Patrol.Type.MechanismSpec.spec
  Patrol.Type.PlatformSpec.spec
  Patrol.Type.ResponseSpec.spec
