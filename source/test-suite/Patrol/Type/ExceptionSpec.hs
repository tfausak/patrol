{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ExceptionSpec where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.Exception as Exception
import qualified Patrol.Type.Mechanism as Mechanism
import qualified Patrol.Type.Stacktrace as Stacktrace
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Exception" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let exception = Exception.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON exception `Hspec.shouldBe` json

    Hspec.it "works with a mechanism" $ do
      let mechanism = Mechanism.empty {Mechanism.type_ = Text.pack "example-mechanism"}
          exception = Exception.empty {Exception.mechanism = Just mechanism}
          json = [Aeson.aesonQQ| { "mechanism": { "type": "example-mechanism" } } |]
      Aeson.toJSON exception `Hspec.shouldBe` json

    Hspec.it "works with a module" $ do
      let exception = Exception.empty {Exception.module_ = Text.pack "example-module"}
          json = [Aeson.aesonQQ| { "module": "example-module" } |]
      Aeson.toJSON exception `Hspec.shouldBe` json

    Hspec.it "works with a stack trace" $ do
      let stacktrace = Stacktrace.empty {Stacktrace.registers = Map.singleton (Text.pack "example-key") $ Text.pack "example-value"}
          exception = Exception.empty {Exception.stacktrace = Just stacktrace}
          json = [Aeson.aesonQQ| { "stacktrace": { "registers": { "example-key": "example-value" } } } |]
      Aeson.toJSON exception `Hspec.shouldBe` json

    Hspec.it "works with a thread ID" $ do
      let exception = Exception.empty {Exception.threadId = Text.pack "example-thread-id"}
          json = [Aeson.aesonQQ| { "thread_id": "example-thread-id" } |]
      Aeson.toJSON exception `Hspec.shouldBe` json

    Hspec.it "works with a type" $ do
      let exception = Exception.empty {Exception.type_ = Text.pack "example-type"}
          json = [Aeson.aesonQQ| { "type": "example-type" } |]
      Aeson.toJSON exception `Hspec.shouldBe` json

    Hspec.it "works with a value" $ do
      let exception = Exception.empty {Exception.value = Text.pack "example-value"}
          json = [Aeson.aesonQQ| { "value": "example-value" } |]
      Aeson.toJSON exception `Hspec.shouldBe` json

  Hspec.describe "fromSomeException" $ do
    let exception = Exception.fromSomeException . Catch.toException $ userError "example-exception-value"

    Hspec.it "sets the type" $ do
      Exception.type_ exception `Hspec.shouldBe` Text.pack "IOException"

    Hspec.it "sets the value" $ do
      Exception.value exception `Hspec.shouldBe` Text.pack "user error (example-exception-value)"
