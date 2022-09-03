module PatrolSpec where

import qualified Patrol.ConstantSpec
import qualified Patrol.Extra.ListSpec
import qualified Patrol.Extra.TextSpec
import qualified Patrol.Type.DsnSpec
import qualified Patrol.Type.Event.IdSpec
import qualified Patrol.Type.EventSpec
import qualified Patrol.Type.HostSpec
import qualified Patrol.Type.PathSpec
import qualified Patrol.Type.PortSpec
import qualified Patrol.Type.ProjectIdSpec
import qualified Patrol.Type.ProtocolSpec
import qualified Patrol.Type.PublicKeySpec
import qualified Patrol.Type.ResponseSpec
import qualified Patrol.Type.SecretKeySpec
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do
  Patrol.ConstantSpec.spec
  Patrol.Extra.ListSpec.spec
  Patrol.Extra.TextSpec.spec
  Patrol.Type.DsnSpec.spec
  Patrol.Type.Event.IdSpec.spec
  Patrol.Type.EventSpec.spec
  Patrol.Type.HostSpec.spec
  Patrol.Type.PathSpec.spec
  Patrol.Type.PortSpec.spec
  Patrol.Type.ProjectIdSpec.spec
  Patrol.Type.ProtocolSpec.spec
  Patrol.Type.PublicKeySpec.spec
  Patrol.Type.ResponseSpec.spec
  Patrol.Type.SecretKeySpec.spec
