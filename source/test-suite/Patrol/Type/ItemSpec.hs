{-# LANGUAGE OverloadedStrings #-}

module Patrol.Type.ItemSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Patrol.Type.Event as Event
import qualified Patrol.Type.Headers as Headers
import qualified Patrol.Type.Item as Item
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Item" $ do
  Hspec.describe "serialize" $ do
    Hspec.it "works with an empty payload" $ do
      let actual =
            Builder.toLazyByteString $
              Item.serialize
                Item.Item
                  { Item.headers = Headers.empty,
                    Item.payload = ""
                  }
      actual `Hspec.shouldBe` "{}\n"

    Hspec.it "works with a non-empty payload" $ do
      let actual =
            Builder.toLazyByteString $
              Item.serialize
                Item.Item
                  { Item.headers = Headers.empty,
                    Item.payload = "x"
                  }
      actual `Hspec.shouldBe` "{}\nx"

    Hspec.it "allows newlines in the payload" $ do
      -- <https://develop.sentry.dev/sdk/data-model/envelopes/#items>
      -- If no `length` is specified, the payload implicitly goes to the next
      -- newline. For payloads containing newline characters, the `length` must
      -- be specified.
      let actual =
            Builder.toLazyByteString $
              Item.serialize
                Item.Item
                  { Item.headers =
                      Headers.fromObject
                        . KeyMap.singleton "length"
                        $ Aeson.toJSON (3 :: Int),
                    Item.payload = "a\nb"
                  }
      actual `Hspec.shouldBe` "{\"length\":3}\na\nb"

  Hspec.describe "fromEvent" $ do
    Hspec.it "sets the headers" $ do
      let event = Event.empty
      let actual = Item.fromEvent event
      let expected =
            Headers.fromObject $
              KeyMap.fromList
                [ ("type", "event"),
                  ("length", Aeson.toJSON (47 :: Int)),
                  ("event_id", Aeson.toJSON $ Event.eventId event)
                ]
      Item.headers actual `Hspec.shouldBe` expected

    Hspec.it "sets the payload" $ do
      let event = Event.empty
      let actual = Item.fromEvent event
      let expected = "{\"event_id\":\"00000000000000000000000000000000\"}" :: ByteString.ByteString
      Item.payload actual `Hspec.shouldBe` expected
