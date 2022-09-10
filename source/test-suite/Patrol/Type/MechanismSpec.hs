{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.MechanismSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.Mechanism as Mechanism
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Mechanism" $ do
  Hspec.describe "ToJSON" $ do
    let emptyMechanism =
          Mechanism.Mechanism
            { Mechanism.data_ = Aeson.Null,
              Mechanism.description = Nothing,
              Mechanism.handled = Nothing,
              Mechanism.helpLink = Nothing,
              Mechanism.synthetic = Nothing,
              Mechanism.type_ = Text.pack "example-type"
            }

    Hspec.it "works" $ do
      let mechanism = emptyMechanism
          json = [Aeson.aesonQQ| { "type": "example-type" } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json

    Hspec.it "works with some data" $ do
      let mechanism = emptyMechanism {Mechanism.data_ = Aeson.Bool True}
          json = [Aeson.aesonQQ| { "type": "example-type", "data": true } |]
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

    Hspec.it "works with a synthetic flag" $ do
      let mechanism = emptyMechanism {Mechanism.synthetic = Just True}
          json = [Aeson.aesonQQ| { "type": "example-type", "synthetic": true } |]
      Aeson.toJSON mechanism `Hspec.shouldBe` json
