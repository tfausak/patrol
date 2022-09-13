{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.LogEntrySpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.LogEntry as LogEntry
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.LogEntry" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let logEntry = LogEntry.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON logEntry `Hspec.shouldBe` json

    Hspec.it "works with a formatted string" $ do
      let logEntry = LogEntry.empty {LogEntry.formatted = Just $ Text.pack "example-formatted"}
          json = [Aeson.aesonQQ| { "formatted": "example-formatted" } |]
      Aeson.toJSON logEntry `Hspec.shouldBe` json

    Hspec.it "works with a message" $ do
      let logEntry = LogEntry.empty {LogEntry.message = Just $ Text.pack "example-message"}
          json = [Aeson.aesonQQ| { "message": "example-message" } |]
      Aeson.toJSON logEntry `Hspec.shouldBe` json

    Hspec.it "works with some params" $ do
      let logEntry = LogEntry.empty {LogEntry.params = Aeson.Bool True}
          json = [Aeson.aesonQQ| { "params": true } |]
      Aeson.toJSON logEntry `Hspec.shouldBe` json
