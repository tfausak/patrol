{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.EventProcessingErrorSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.ErrorType as ErrorType
import qualified Patrol.Type.EventProcessingError as EventProcessingError
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.EventProcessingError" $ do
  Hspec.describe "ToJSON" $ do
    let emptyEventProcessingError =
          EventProcessingError.EventProcessingError
            { EventProcessingError.name = Nothing,
              EventProcessingError.type_ = ErrorType.UnknownError,
              EventProcessingError.value = Aeson.Null
            }

    Hspec.it "works" $ do
      let eventProcessingError = emptyEventProcessingError
          json = [Aeson.aesonQQ| { "type": "unknown_error" } |]
      Aeson.toJSON eventProcessingError `Hspec.shouldBe` json

    Hspec.it "works with a name" $ do
      let eventProcessingError = emptyEventProcessingError {EventProcessingError.name = Just $ Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "type": "unknown_error", "name": "example-name" } |]
      Aeson.toJSON eventProcessingError `Hspec.shouldBe` json

    Hspec.it "works with a value" $ do
      let eventProcessingError = emptyEventProcessingError {EventProcessingError.value = Aeson.Bool True}
          json = [Aeson.aesonQQ| { "type": "unknown_error", "value": true } |]
      Aeson.toJSON eventProcessingError `Hspec.shouldBe` json
