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

    Hspec.it "works for Debug" $ do
      Aeson.toJSON BreadcrumbType.Debug `Hspec.shouldBe` [Aeson.aesonQQ| "debug" |]

    Hspec.it "works for Error" $ do
      Aeson.toJSON BreadcrumbType.Error `Hspec.shouldBe` [Aeson.aesonQQ| "error" |]

    Hspec.it "works for Navigation" $ do
      Aeson.toJSON BreadcrumbType.Navigation `Hspec.shouldBe` [Aeson.aesonQQ| "navigation" |]

    Hspec.it "works for Http" $ do
      Aeson.toJSON BreadcrumbType.Http `Hspec.shouldBe` [Aeson.aesonQQ| "http" |]

    Hspec.it "works for Info" $ do
      Aeson.toJSON BreadcrumbType.Info `Hspec.shouldBe` [Aeson.aesonQQ| "info" |]

    Hspec.it "works for Query" $ do
      Aeson.toJSON BreadcrumbType.Query `Hspec.shouldBe` [Aeson.aesonQQ| "query" |]

    Hspec.it "works for Transaction" $ do
      Aeson.toJSON BreadcrumbType.Transaction `Hspec.shouldBe` [Aeson.aesonQQ| "transaction" |]

    Hspec.it "works for UI" $ do
      Aeson.toJSON BreadcrumbType.UI `Hspec.shouldBe` [Aeson.aesonQQ| "ui" |]

    Hspec.it "works for User" $ do
      Aeson.toJSON BreadcrumbType.User `Hspec.shouldBe` [Aeson.aesonQQ| "user" |]
