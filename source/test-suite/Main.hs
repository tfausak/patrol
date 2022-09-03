import qualified PatrolSpec
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec $ Hspec.parallel PatrolSpec.spec
