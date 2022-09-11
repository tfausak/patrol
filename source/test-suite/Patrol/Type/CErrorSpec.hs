{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.CErrorSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.CError as CError
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.CError" $ do
  Hspec.describe "ToJSON" $ do
    let emptyCError =
          CError.CError
            { CError.name = Nothing,
              CError.number = Nothing
            }

    Hspec.it "works" $ do
      let cError = emptyCError
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON cError `Hspec.shouldBe` json

    Hspec.it "works with a name" $ do
      let cError = emptyCError {CError.name = Just $ Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON cError `Hspec.shouldBe` json

    Hspec.it "works with a number" $ do
      let cError = emptyCError {CError.number = Just 0}
          json = [Aeson.aesonQQ| { "number": 0 } |]
      Aeson.toJSON cError `Hspec.shouldBe` json
