module Patrol.Type.PortSpec where

import qualified Patrol.Type.Port as Port
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Port" $ do
  Hspec.describe "fromNatural" $ do
    Hspec.it "converts from natural" $ do
      Port.fromNatural 0 `Hspec.shouldBe` Port.Port 0

  Hspec.describe "intoNatural" $ do
    Hspec.it "converts into natural" $ do
      Port.intoNatural (Port.Port 0) `Hspec.shouldBe` 0
