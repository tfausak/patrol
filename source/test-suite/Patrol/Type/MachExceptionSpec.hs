{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.MachExceptionSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.MachException as MachException
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.MachException" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let machException = MachException.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON machException `Hspec.shouldBe` json

    Hspec.it "works with a code" $ do
      let machException = MachException.empty {MachException.code = Just 0}
          json = [Aeson.aesonQQ| { "code": 0 } |]
      Aeson.toJSON machException `Hspec.shouldBe` json

    Hspec.it "works with an exception" $ do
      let machException = MachException.empty {MachException.exception = Just 0}
          json = [Aeson.aesonQQ| { "exception": 0 } |]
      Aeson.toJSON machException `Hspec.shouldBe` json

    Hspec.it "works with a name" $ do
      let machException = MachException.empty {MachException.name = Just $ Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON machException `Hspec.shouldBe` json

    Hspec.it "works with a subcode" $ do
      let machException = MachException.empty {MachException.subcode = Just 0}
          json = [Aeson.aesonQQ| { "subcode": 0 } |]
      Aeson.toJSON machException `Hspec.shouldBe` json
