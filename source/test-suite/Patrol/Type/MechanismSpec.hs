{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.MechanismSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.CError as CError
import qualified Patrol.Type.Mechanism as Mechanism
import qualified Patrol.Type.MechanismMeta as MechanismMeta
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Mechanism" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let mechanism = Mechanism.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json

    Hspec.it "works with some data" $ do
      let mechanism = Mechanism.empty {Mechanism.data_ = Map.singleton (Text.pack "example-data") Aeson.Null}
          json = [Aeson.aesonQQ| { "data": { "example-data": null } } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json

    Hspec.it "works with a description" $ do
      let mechanism = Mechanism.empty {Mechanism.description = Just $ Text.pack "example-description"}
          json = [Aeson.aesonQQ| { "description": "example-description" } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json

    Hspec.it "works with a handled flag" $ do
      let mechanism = Mechanism.empty {Mechanism.handled = Just True}
          json = [Aeson.aesonQQ| { "handled": true } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json

    Hspec.it "works with a help link" $ do
      let mechanism = Mechanism.empty {Mechanism.helpLink = Just $ Text.pack "example-help-link"}
          json = [Aeson.aesonQQ| { "help_link": "example-help-link" } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json

    Hspec.it "works with some meta" $ do
      let cError = CError.empty {CError.number = Just 0}
          mechanismMeta = MechanismMeta.empty {MechanismMeta.errno = Just cError}
          mechanism = Mechanism.empty {Mechanism.meta = Just mechanismMeta}
          json = [Aeson.aesonQQ| { "meta": { "errno": { "number": 0 } } } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json

    Hspec.it "works with a synthetic flag" $ do
      let mechanism = Mechanism.empty {Mechanism.synthetic = Just True}
          json = [Aeson.aesonQQ| { "synthetic": true } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json

    Hspec.it "works with a type" $ do
      let mechanism = Mechanism.empty {Mechanism.type_ = Just $ Text.pack "example-type"}
          json = [Aeson.aesonQQ| { "type": "example-type" } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json
