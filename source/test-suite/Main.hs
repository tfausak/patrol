import qualified Patrol.Extra.TextSpec
import qualified Patrol.Type.DsnSpec
import qualified Patrol.Type.HostSpec
import qualified Patrol.Type.PathSpec
import qualified Patrol.Type.PortSpec
import qualified Patrol.Type.ProjectIdSpec
import qualified Patrol.Type.ProtocolSpec
import qualified Patrol.Type.PublicKeySpec
import qualified Patrol.Type.SecretKeySpec
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec $ Hspec.parallel spec

spec :: Hspec.Spec
spec = do
  Patrol.Extra.TextSpec.spec
  Patrol.Type.DsnSpec.spec
  Patrol.Type.HostSpec.spec
  Patrol.Type.PathSpec.spec
  Patrol.Type.PortSpec.spec
  Patrol.Type.ProjectIdSpec.spec
  Patrol.Type.ProtocolSpec.spec
  Patrol.Type.PublicKeySpec.spec
  Patrol.Type.SecretKeySpec.spec
