{-# LANGUAGE OverloadedStrings #-}

module Patrol.Type.ItemSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Builder as Builder
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
                  { Item.headers = Headers.fromObject mempty,
                    Item.payload = ""
                  }
      actual `Hspec.shouldBe` "{}\n"

    Hspec.it "works with a non-empty payload" $ do
      let actual =
            Builder.toLazyByteString $
              Item.serialize
                Item.Item
                  { Item.headers = Headers.fromObject mempty,
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
