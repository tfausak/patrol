module Patrol.ConstantSpec where

import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Patrol.Constant as Constant
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Constant" $ do
  Hspec.describe "applicationJson" $ do
    Hspec.it "is correct" $ do
      Constant.applicationJson `Hspec.shouldBe` Text.encodeUtf8 (Text.pack "application/json")

  Hspec.describe "sentryVersion" $ do
    Hspec.it "is correct" $ do
      Constant.sentryVersion `Hspec.shouldBe` Text.singleton '7'

  Hspec.describe "userAgent" $ do
    Hspec.it "is correct" $ do
      Constant.userAgent `Hspec.shouldSatisfy` Text.isPrefixOf (Text.pack "patrol/")

  Hspec.describe "xSentryAuth" $ do
    Hspec.it "is correct" $ do
      Constant.xSentryAuth `Hspec.shouldBe` CI.mk (Text.encodeUtf8 $ Text.pack "X-Sentry-Auth")
