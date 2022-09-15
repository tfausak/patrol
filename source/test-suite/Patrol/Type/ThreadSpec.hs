{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ThreadSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.Stacktrace as Stacktrace
import qualified Patrol.Type.Thread as Thread
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Thread" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let thread = Thread.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON thread `Hspec.shouldBe` json

    Hspec.it "works with a crashed flag" $ do
      let thread = Thread.empty {Thread.crashed = Just True}
          json = [Aeson.aesonQQ| { "crashed": true } |]
      Aeson.toJSON thread `Hspec.shouldBe` json

    Hspec.it "works with a current flag" $ do
      let thread = Thread.empty {Thread.current = Just True}
          json = [Aeson.aesonQQ| { "current": true } |]
      Aeson.toJSON thread `Hspec.shouldBe` json

    Hspec.it "works with an ID" $ do
      let thread = Thread.empty {Thread.id = Text.pack "example-id"}
          json = [Aeson.aesonQQ| { "id": "example-id" } |]
      Aeson.toJSON thread `Hspec.shouldBe` json

    Hspec.it "works with a name" $ do
      let thread = Thread.empty {Thread.name = Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON thread `Hspec.shouldBe` json

    Hspec.it "works with a stack trace" $ do
      let stacktrace = Stacktrace.empty {Stacktrace.registers = Map.singleton (Text.pack "example-register") (Aeson.Bool True)}
          thread = Thread.empty {Thread.stacktrace = Just stacktrace}
          json = [Aeson.aesonQQ| { "stacktrace": { "registers": { "example-register": true } } } |]
      Aeson.toJSON thread `Hspec.shouldBe` json
