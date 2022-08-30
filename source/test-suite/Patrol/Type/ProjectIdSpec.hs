module Patrol.Type.ProjectIdSpec where

import qualified Data.Text as Text
import qualified Patrol.Type.ProjectId as ProjectId
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.ProjectId" $ do
  Hspec.describe "fromText" $ do
    Hspec.it "fails when the text is empty" $ do
      ProjectId.fromText Text.empty `Hspec.shouldBe` Nothing

    Hspec.it "succeeds when the text is not empty" $ do
      let text = Text.singleton ' '
      ProjectId.fromText text `Hspec.shouldBe` Just (ProjectId.ProjectId text)

  Hspec.describe "toText" $ do
    Hspec.it "converts into text" $ do
      let text = Text.singleton ' '
      ProjectId.toText (ProjectId.ProjectId text) `Hspec.shouldBe` text
