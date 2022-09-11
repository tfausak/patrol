{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ValueClassSpec where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.Mechanism as Mechanism
import qualified Patrol.Type.Stacktrace as Stacktrace
import qualified Patrol.Type.ValueClass as ValueClass
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.ValueClass" $ do
  Hspec.describe "ToJSON" $ do
    let emptyValueClass =
          ValueClass.ValueClass
            { ValueClass.mechanism = Nothing,
              ValueClass.module_ = Nothing,
              ValueClass.stacktrace = Nothing,
              ValueClass.threadId = Nothing,
              ValueClass.type_ = Nothing,
              ValueClass.value = Nothing
            }

    Hspec.it "works" $ do
      let valueClass = emptyValueClass
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON valueClass `Hspec.shouldBe` json

    Hspec.it "works with a mechanism" $ do
      let mechanism =
            Mechanism.Mechanism
              { Mechanism.data_ = Map.empty,
                Mechanism.description = Nothing,
                Mechanism.handled = Nothing,
                Mechanism.helpLink = Nothing,
                Mechanism.meta = Nothing,
                Mechanism.synthetic = Nothing,
                Mechanism.type_ = Text.pack "example-mechanism"
              }
          valueClass = emptyValueClass {ValueClass.mechanism = Just mechanism}
          json = [Aeson.aesonQQ| { "mechanism": { "type": "example-mechanism" } } |]
      Aeson.toJSON valueClass `Hspec.shouldBe` json

    Hspec.it "works with a module" $ do
      let valueClass = emptyValueClass {ValueClass.module_ = Just $ Text.pack "example-module"}
          json = [Aeson.aesonQQ| { "module": "example-module" } |]
      Aeson.toJSON valueClass `Hspec.shouldBe` json

    Hspec.it "works with a stack trace" $ do
      let stacktrace =
            Stacktrace.Stacktrace
              { Stacktrace.frames = [],
                Stacktrace.registers = Map.singleton (Text.pack "example-register") Aeson.Null
              }
          valueClass = emptyValueClass {ValueClass.stacktrace = Just stacktrace}
          json = [Aeson.aesonQQ| { "stacktrace": { "registers": { "example-register": null } } } |]
      Aeson.toJSON valueClass `Hspec.shouldBe` json

    Hspec.it "works with a thread ID" $ do
      let valueClass = emptyValueClass {ValueClass.threadId = Just $ Text.pack "example-thread-id"}
          json = [Aeson.aesonQQ| { "thread_id": "example-thread-id" } |]
      Aeson.toJSON valueClass `Hspec.shouldBe` json

    Hspec.it "works with a type" $ do
      let valueClass = emptyValueClass {ValueClass.type_ = Just $ Text.pack "example-type"}
          json = [Aeson.aesonQQ| { "type": "example-type" } |]
      Aeson.toJSON valueClass `Hspec.shouldBe` json

    Hspec.it "works with a value" $ do
      let valueClass = emptyValueClass {ValueClass.value = Just $ Text.pack "example-value"}
          json = [Aeson.aesonQQ| { "value": "example-value" } |]
      Aeson.toJSON valueClass `Hspec.shouldBe` json

  Hspec.describe "fromSomeException" $ do
    let valueClass = ValueClass.fromSomeException . Catch.toException $ userError "example-value-class"

    Hspec.it "does not set the mechanism" $ do
      ValueClass.mechanism valueClass `Hspec.shouldBe` Nothing

    Hspec.it "does not set the module" $ do
      ValueClass.module_ valueClass `Hspec.shouldBe` Nothing

    Hspec.it "does not set the stack trace" $ do
      ValueClass.stacktrace valueClass `Hspec.shouldBe` Nothing

    Hspec.it "does not set the thread ID" $ do
      ValueClass.threadId valueClass `Hspec.shouldBe` Nothing

    Hspec.it "sets the type" $ do
      ValueClass.type_ valueClass `Hspec.shouldBe` Just (Text.pack "IOException")

    Hspec.it "sets the value" $ do
      ValueClass.value valueClass `Hspec.shouldBe` Just (Text.pack "user error (example-value-class)")
