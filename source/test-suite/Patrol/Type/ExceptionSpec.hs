{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ExceptionSpec where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.Exception as Exception
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Exception" $ do
  Hspec.describe "ToJSON" $ do
    let emptyException =
          Exception.Exception
            { Exception.module_ = Nothing,
              Exception.type_ = Text.pack "example-type",
              Exception.value = Nothing
            }

    Hspec.it "works" $ do
      Aeson.toJSON emptyException `Hspec.shouldBe` [Aeson.aesonQQ| { "type": "example-type" } |]

    Hspec.it "works with a module" $ do
      Aeson.toJSON emptyException {Exception.module_ = Just $ Text.pack "example-module"} `Hspec.shouldBe` [Aeson.aesonQQ| { "type": "example-type", "module": "example-module" } |]

    Hspec.it "works with a value" $ do
      Aeson.toJSON emptyException {Exception.value = Just $ Text.pack "example-value"} `Hspec.shouldBe` [Aeson.aesonQQ| { "type": "example-type", "value": "example-value" } |]

  Hspec.describe "fromSomeException" $ do
    let exception = Exception.fromSomeException . Catch.toException $ userError "example-exception"

    Hspec.it "does not set the module" $ do
      Exception.module_ exception `Hspec.shouldBe` Nothing

    Hspec.it "sets the type" $ do
      Exception.type_ exception `Hspec.shouldBe` Text.pack "IOException"

    Hspec.it "sets the value" $ do
      Exception.value exception `Hspec.shouldBe` Just (Text.pack "user error (example-exception)")
