{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ErrorSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.Error as Error
import qualified Patrol.Type.ErrorType as ErrorType
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Error" $ do
  Hspec.describe "ToJSON" $ do
    let emptyError =
          Error.Error
            { Error.name = Nothing,
              Error.type_ = ErrorType.UnknownError,
              Error.value = Aeson.Null
            }

    Hspec.it "works" $ do
      let error_ = emptyError
          json = [Aeson.aesonQQ| { "type": "unknown_error" } |]
      Aeson.toJSON error_ `Hspec.shouldBe` json

    Hspec.it "works with a name" $ do
      let error_ = emptyError {Error.name = Just $ Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "type": "unknown_error", "name": "example-name" } |]
      Aeson.toJSON error_ `Hspec.shouldBe` json

    Hspec.it "works with a value" $ do
      let error_ = emptyError {Error.value = Aeson.Bool True}
          json = [Aeson.aesonQQ| { "type": "unknown_error", "value": true } |]
      Aeson.toJSON error_ `Hspec.shouldBe` json
