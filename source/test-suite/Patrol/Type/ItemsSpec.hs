{-# LANGUAGE OverloadedStrings #-}

module Patrol.Type.ItemsSpec where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Builder as Builder
import qualified Patrol.Type.Event as Event
import qualified Patrol.Type.Item as Item
import qualified Patrol.Type.Items as Items
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Items" $ do
  Hspec.describe "serialize" $ do
    Hspec.it "works on a raw binary blob" $ do
      let actual =
            LazyByteString.toStrict $
              Builder.toLazyByteString $
                Items.serialize $ Items.Raw "test"
      actual `Hspec.shouldBe` "test"

    Hspec.it "works with a single Event" $ do
      let actual =
            LazyByteString.toStrict $
              Builder.toLazyByteString $
                Items.serialize $ Items.EnvelopeItems [Item.Event Event.empty]
      actual `Hspec.shouldBe` "{\"type\":\"event\",\"length\":47}\n{\"event_id\":\"00000000000000000000000000000000\"}\n"
