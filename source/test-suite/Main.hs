import Patrol ()
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec . Hspec.describe "Patrol" $ do
  Hspec.it "" Hspec.pending
