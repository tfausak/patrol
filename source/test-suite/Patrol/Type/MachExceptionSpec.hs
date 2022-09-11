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
    let emptyMachException =
          MachException.MachException
            { MachException.code = 1,
              MachException.exception = 0,
              MachException.subcode = 2,
              MachException.name = Nothing
            }

    Hspec.it "works" $ do
      let machException = emptyMachException
          json = [Aeson.aesonQQ| { "exception": 0, "code": 1, "subcode": 2 } |]
      Aeson.toJSON machException `Hspec.shouldBe` json

    Hspec.it "works with a name" $ do
      let machException = emptyMachException {MachException.name = Just $ Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "exception": 0, "code": 1, "subcode": 2, "name": "example-name" } |]
      Aeson.toJSON machException `Hspec.shouldBe` json
