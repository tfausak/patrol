{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.DiscardedEventSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Patrol.Type.DataCategory as DataCategory
import qualified Patrol.Type.DiscardedEvent as DiscardedEvent
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.DiscardedEvent" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "serializes all fields" $ do
      let discardedEvent =
            DiscardedEvent.DiscardedEvent
              { DiscardedEvent.reason = "sample_rate",
                DiscardedEvent.category = DataCategory.Error,
                DiscardedEvent.quantity = 3
              }
      Aeson.toJSON discardedEvent
        `Hspec.shouldBe` [Aeson.aesonQQ| {"reason": "sample_rate", "category": "error", "quantity": 3} |]

    Hspec.it "serializes quantity of 0" $ do
      let discardedEvent =
            DiscardedEvent.DiscardedEvent
              { DiscardedEvent.reason = "before_send",
                DiscardedEvent.category = DataCategory.Transaction,
                DiscardedEvent.quantity = 0
              }
      Aeson.toJSON discardedEvent
        `Hspec.shouldBe` [Aeson.aesonQQ| {"reason": "before_send", "category": "transaction", "quantity": 0} |]
