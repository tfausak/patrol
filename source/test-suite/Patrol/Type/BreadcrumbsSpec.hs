{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.BreadcrumbsSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.Breadcrumb as Breadcrumb
import qualified Patrol.Type.Breadcrumbs as Breadcrumbs
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Breadcrumbs" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let breadcrumbs = Breadcrumbs.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON breadcrumbs `Hspec.shouldBe` json

    Hspec.it "works with a value" $ do
      let breadcrumb = Breadcrumb.empty {Breadcrumb.category = Text.pack "example-category"}
          breadcrumbs = Breadcrumbs.empty {Breadcrumbs.values = [breadcrumb]}
          json = [Aeson.aesonQQ| { "values": [ { "category": "example-category" } ] } |]
      Aeson.toJSON breadcrumbs `Hspec.shouldBe` json
