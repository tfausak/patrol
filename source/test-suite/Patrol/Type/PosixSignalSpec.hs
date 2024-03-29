{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.PosixSignalSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.PosixSignal as PosixSignal
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.PosixSignal" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let posixSignal = PosixSignal.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON posixSignal `Hspec.shouldBe` json

    Hspec.it "works with a code" $ do
      let posixSignal = PosixSignal.empty {PosixSignal.code = Just 0}
          json = [Aeson.aesonQQ| { "code": 0 } |]
      Aeson.toJSON posixSignal `Hspec.shouldBe` json

    Hspec.it "works with a code name" $ do
      let posixSignal = PosixSignal.empty {PosixSignal.codeName = Text.pack "example-code-name"}
          json = [Aeson.aesonQQ| { "code_name": "example-code-name" } |]
      Aeson.toJSON posixSignal `Hspec.shouldBe` json

    Hspec.it "works with a name" $ do
      let posixSignal = PosixSignal.empty {PosixSignal.name = Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON posixSignal `Hspec.shouldBe` json

    Hspec.it "works with a number" $ do
      let posixSignal = PosixSignal.empty {PosixSignal.number = Just 0}
          json = [Aeson.aesonQQ| { "number": 0 } |]
      Aeson.toJSON posixSignal `Hspec.shouldBe` json
