{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ExceptionValueSpec where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.ExceptionValue as ExceptionValue
import qualified Patrol.Type.Mechanism as Mechanism
import qualified Patrol.Type.Stacktrace as Stacktrace
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.ExceptionValue" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let exceptionValue = ExceptionValue.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON exceptionValue `Hspec.shouldBe` json

    Hspec.it "works with a mechanism" $ do
      let mechanism = Mechanism.empty {Mechanism.type_ = Just $ Text.pack "example-mechanism"}
          exceptionValue = ExceptionValue.empty {ExceptionValue.mechanism = Just mechanism}
          json = [Aeson.aesonQQ| { "mechanism": { "type": "example-mechanism" } } |]
      Aeson.toJSON exceptionValue `Hspec.shouldBe` json

    Hspec.it "works with a module" $ do
      let exceptionValue = ExceptionValue.empty {ExceptionValue.module_ = Just $ Text.pack "example-module"}
          json = [Aeson.aesonQQ| { "module": "example-module" } |]
      Aeson.toJSON exceptionValue `Hspec.shouldBe` json

    Hspec.it "works with a stack trace" $ do
      let stacktrace = Stacktrace.empty {Stacktrace.registers = Map.singleton (Text.pack "example-register") Aeson.Null}
          exceptionValue = ExceptionValue.empty {ExceptionValue.stacktrace = Just stacktrace}
          json = [Aeson.aesonQQ| { "stacktrace": { "registers": { "example-register": null } } } |]
      Aeson.toJSON exceptionValue `Hspec.shouldBe` json

    Hspec.it "works with a thread ID" $ do
      let exceptionValue = ExceptionValue.empty {ExceptionValue.threadId = Just $ Text.pack "example-thread-id"}
          json = [Aeson.aesonQQ| { "thread_id": "example-thread-id" } |]
      Aeson.toJSON exceptionValue `Hspec.shouldBe` json

    Hspec.it "works with a type" $ do
      let exceptionValue = ExceptionValue.empty {ExceptionValue.type_ = Just $ Text.pack "example-type"}
          json = [Aeson.aesonQQ| { "type": "example-type" } |]
      Aeson.toJSON exceptionValue `Hspec.shouldBe` json

    Hspec.it "works with a value" $ do
      let exceptionValue = ExceptionValue.empty {ExceptionValue.value = Just $ Text.pack "example-value"}
          json = [Aeson.aesonQQ| { "value": "example-value" } |]
      Aeson.toJSON exceptionValue `Hspec.shouldBe` json

  Hspec.describe "fromSomeException" $ do
    let exceptionValue = ExceptionValue.fromSomeException . Catch.toException $ userError "example-exception-value"

    Hspec.it "does not set the mechanism" $ do
      ExceptionValue.mechanism exceptionValue `Hspec.shouldBe` Nothing

    Hspec.it "does not set the module" $ do
      ExceptionValue.module_ exceptionValue `Hspec.shouldBe` Nothing

    Hspec.it "does not set the stack trace" $ do
      ExceptionValue.stacktrace exceptionValue `Hspec.shouldBe` Nothing

    Hspec.it "does not set the thread ID" $ do
      ExceptionValue.threadId exceptionValue `Hspec.shouldBe` Nothing

    Hspec.it "sets the type" $ do
      ExceptionValue.type_ exceptionValue `Hspec.shouldBe` Just (Text.pack "IOException")

    Hspec.it "sets the value" $ do
      ExceptionValue.value exceptionValue `Hspec.shouldBe` Just (Text.pack "user error (example-exception-value)")
