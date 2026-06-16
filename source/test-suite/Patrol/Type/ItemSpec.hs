{-# LANGUAGE OverloadedStrings #-}

module Patrol.Type.ItemSpec where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Patrol.Type.ClientReport as ClientReport
import qualified Patrol.Type.Event as Event
import qualified Patrol.Type.Item as Item
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Item" $ do
  Hspec.describe "serialize" $ do
    Hspec.it "works on Event" $ do
      let actual =
            LazyByteString.toStrict
              . Builder.toLazyByteString
              . Item.serialize
              $ Item.Event Event.empty
      let expected = "{\"type\":\"event\",\"length\":47}\n{\"event_id\":\"00000000000000000000000000000000\"}" :: ByteString.ByteString
      actual `Hspec.shouldBe` expected

    Hspec.it "works on ClientReport" $ do
      let actual =
            LazyByteString.toStrict
              . Builder.toLazyByteString
              . Item.serialize
              $ Item.ClientReport ClientReport.empty
      let expected = "{\"type\":\"client_report\",\"length\":2}\n{}" :: ByteString.ByteString
      actual `Hspec.shouldBe` expected

    Hspec.it "works on Raw" $ do
      let actual =
            LazyByteString.toStrict
              . Builder.toLazyByteString
              . Item.serialize
              $ Item.Raw
      let expected = "" :: ByteString.ByteString
      actual `Hspec.shouldBe` expected
