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
    let emptyMechanism =
          Mechanism.Mechanism
            { Mechanism.data_ = Map.empty,
              Mechanism.description = Nothing,
              Mechanism.handled = Nothing,
              Mechanism.helpLink = Nothing,
              Mechanism.meta = Nothing,
              Mechanism.synthetic = Nothing,
              Mechanism.type_ = Text.pack "example-type"
            }

    Hspec.it "works" $ do
      let mechanism = emptyMechanism
          json = [Aeson.aesonQQ| { "type": "example-type" } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json

    Hspec.it "works with some data" $ do
      let mechanism = emptyMechanism {Mechanism.data_ = Map.singleton (Text.pack "example-data") Aeson.Null}
          json = [Aeson.aesonQQ| { "type": "example-type", "data": { "example-data": null } } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json

    Hspec.it "works with a description" $ do
      let mechanism = emptyMechanism {Mechanism.description = Just $ Text.pack "example-description"}
          json = [Aeson.aesonQQ| { "type": "example-type", "description": "example-description" } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json

    Hspec.it "works with a handled flag" $ do
      let mechanism = emptyMechanism {Mechanism.handled = Just True}
          json = [Aeson.aesonQQ| { "type": "example-type", "handled": true } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json

    Hspec.it "works with a help link" $ do
      let mechanism = emptyMechanism {Mechanism.helpLink = Just $ Text.pack "example-help-link"}
          json = [Aeson.aesonQQ| { "type": "example-type", "help_link": "example-help-link" } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json

    Hspec.it "works with some meta" $ do
      let cError =
            CError.CError
              { CError.name = Nothing,
                CError.number = Just 0
              }
          mechanismMeta =
            MechanismMeta.MechanismMeta
              { MechanismMeta.errno = Just cError,
                MechanismMeta.machException = Nothing,
                MechanismMeta.nsError = Nothing,
                MechanismMeta.signal = Nothing
              }
          mechanism = emptyMechanism {Mechanism.meta = Just mechanismMeta}
          json = [Aeson.aesonQQ| { "type": "example-type", "meta": { "errno": { "number": 0 } } } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json

    Hspec.it "works with a synthetic flag" $ do
      let mechanism = emptyMechanism {Mechanism.synthetic = Just True}
          json = [Aeson.aesonQQ| { "type": "example-type", "synthetic": true } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json
