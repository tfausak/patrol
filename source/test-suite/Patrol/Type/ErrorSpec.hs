{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ErrorSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.Error as Error
import qualified Patrol.Type.ErrorType as ErrorType
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Error" $ do
  Hspec.describe "ToJSON" $ do
    let emptyError =
          Error.Error
            { Error.type_ = ErrorType.UnknownError,
              Error.value = Map.empty
            }

    Hspec.it "works" $ do
      let error_ = emptyError
          json = [Aeson.aesonQQ| { "type": "unknown_error" } |]
      Aeson.toJSON error_ `Hspec.shouldBe` json

    Hspec.it "overrides the type" $ do
      let error_ = emptyError {Error.value = Map.singleton (Text.pack "type") Aeson.Null}
          json = [Aeson.aesonQQ| { "type": null } |]
      Aeson.toJSON error_ `Hspec.shouldBe` json
