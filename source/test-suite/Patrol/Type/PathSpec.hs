module Patrol.Type.PathSpec where

import qualified Data.Text as Text
import qualified Patrol.Type.Path as Path
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Path" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      Path.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton 'x'
      Path.fromText text `Hspec.shouldBe` Just (Path.Path text)

  Hspec.describe "intoText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton 'x'
      Path.intoText (Path.Path text) `Hspec.shouldBe` text
