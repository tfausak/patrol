module PatrolSpec where

import qualified Patrol.ClientSpec
import qualified Patrol.ConstantSpec
import qualified Patrol.Extra.ListSpec
import qualified Patrol.Extra.TextSpec
import qualified Patrol.Type.DistSpec
import qualified Patrol.Type.DsnSpec
import qualified Patrol.Type.EventIdSpec
import qualified Patrol.Type.EventSpec
import qualified Patrol.Type.HostSpec
import qualified Patrol.Type.LevelSpec
import qualified Patrol.Type.LoggerSpec
import qualified Patrol.Type.PathSpec
import qualified Patrol.Type.PlatformSpec
import qualified Patrol.Type.PortSpec
import qualified Patrol.Type.ProjectIdSpec
import qualified Patrol.Type.ProtocolSpec
import qualified Patrol.Type.PublicKeySpec
import qualified Patrol.Type.ReleaseSpec
import qualified Patrol.Type.ResponseSpec
import qualified Patrol.Type.SecretKeySpec
import qualified Patrol.Type.TimestampSpec
import qualified Patrol.Type.TransactionSpec
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do
  Patrol.ClientSpec.spec
  Patrol.ConstantSpec.spec
  Patrol.Extra.ListSpec.spec
  Patrol.Extra.TextSpec.spec
  Patrol.Type.DistSpec.spec
  Patrol.Type.DsnSpec.spec
  Patrol.Type.EventIdSpec.spec
  Patrol.Type.EventSpec.spec
  Patrol.Type.HostSpec.spec
  Patrol.Type.LevelSpec.spec
  Patrol.Type.LoggerSpec.spec
  Patrol.Type.PathSpec.spec
  Patrol.Type.PlatformSpec.spec
  Patrol.Type.PortSpec.spec
  Patrol.Type.ProjectIdSpec.spec
  Patrol.Type.ProtocolSpec.spec
  Patrol.Type.PublicKeySpec.spec
  Patrol.Type.ReleaseSpec.spec
  Patrol.Type.ResponseSpec.spec
  Patrol.Type.SecretKeySpec.spec
  Patrol.Type.TimestampSpec.spec
  Patrol.Type.TransactionSpec.spec
