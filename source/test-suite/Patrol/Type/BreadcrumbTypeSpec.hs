{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.BreadcrumbTypeSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Patrol.Type.BreadcrumbType as BreadcrumbType
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.BreadcrumbType" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works for Default" $ do
      Aeson.toJSON BreadcrumbType.Default `Hspec.shouldBe` [Aeson.aesonQQ| "default" |]

    Hspec.it "works for Http" $ do
      Aeson.toJSON BreadcrumbType.Http `Hspec.shouldBe` [Aeson.aesonQQ| "http" |]

    Hspec.it "works for Navigation" $ do
      Aeson.toJSON BreadcrumbType.Navigation `Hspec.shouldBe` [Aeson.aesonQQ| "navigation" |]
