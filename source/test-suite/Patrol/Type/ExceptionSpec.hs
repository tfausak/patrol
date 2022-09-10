{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ExceptionSpec where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.Exception as Exception
import qualified Patrol.Type.Mechanism as Mechanism
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Exception" $ do
  Hspec.describe "ToJSON" $ do
    let emptyException =
          Exception.Exception
            { Exception.mechanism = Nothing,
              Exception.module_ = Nothing,
              Exception.threadId = Nothing,
              Exception.type_ = Text.pack "example-type",
              Exception.value = Nothing
            }

    Hspec.it "works" $ do
      let exception = emptyException
          json = [Aeson.aesonQQ| { "type": "example-type" } |]
      Aeson.toJSON exception `Hspec.shouldBe` json

    Hspec.it "works with a mechanism" $ do
      let mechanism =
            Mechanism.Mechanism
              { Mechanism.data_ = Aeson.Null,
                Mechanism.description = Nothing,
                Mechanism.handled = Nothing,
                Mechanism.helpLink = Nothing,
                Mechanism.synthetic = Nothing,
                Mechanism.type_ = Text.pack "example-mechanism"
              }
          exception = emptyException {Exception.mechanism = Just mechanism}
          json = [Aeson.aesonQQ| { "type": "example-type", "mechanism": { "type": "example-mechanism" } } |]
      Aeson.toJSON exception `Hspec.shouldBe` json

    Hspec.it "works with a module" $ do
      let exception = emptyException {Exception.module_ = Just $ Text.pack "example-module"}
          json = [Aeson.aesonQQ| { "type": "example-type", "module": "example-module" } |]
      Aeson.toJSON exception `Hspec.shouldBe` json

    Hspec.it "works with a thread ID" $ do
      let exception = emptyException {Exception.threadId = Just $ Text.pack "example-thread-id"}
          json = [Aeson.aesonQQ| { "type": "example-type", "thread_id": "example-thread-id" } |]
      Aeson.toJSON exception `Hspec.shouldBe` json

    Hspec.it "works with a value" $ do
      let exception = emptyException {Exception.value = Just $ Text.pack "example-value"}
          json = [Aeson.aesonQQ| { "type": "example-type", "value": "example-value" } |]
      Aeson.toJSON exception `Hspec.shouldBe` json

  Hspec.describe "fromSomeException" $ do
    let exception = Exception.fromSomeException . Catch.toException $ userError "example-exception"

    Hspec.it "does not set the module" $ do
      Exception.module_ exception `Hspec.shouldBe` Nothing

    Hspec.it "does not set the thread ID" $ do
      Exception.threadId exception `Hspec.shouldBe` Nothing

    Hspec.it "sets the type" $ do
      Exception.type_ exception `Hspec.shouldBe` Text.pack "IOException"

    Hspec.it "sets the value" $ do
      Exception.value exception `Hspec.shouldBe` Just (Text.pack "user error (example-exception)")
