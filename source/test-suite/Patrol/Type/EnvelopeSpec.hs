{-# LANGUAGE OverloadedStrings #-}

module Patrol.Type.EnvelopeSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Builder as Builder
import qualified Patrol.Type.Envelope as Envelope
import qualified Patrol.Type.Headers as Headers
import qualified Patrol.Type.Item as Item
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Envelope" $ do
  Hspec.describe "serialize" $ do
    Hspec.it "works with no items" $ do
      let actual =
            Builder.toLazyByteString $
              Envelope.serialize
                Envelope.Envelope
                  { Envelope.headers = Headers.fromObject mempty,
                    Envelope.items = []
                  }
      actual `Hspec.shouldBe` "{}\n"

    Hspec.it "works with one item" $ do
      let actual =
            Builder.toLazyByteString $
              Envelope.serialize
                Envelope.Envelope
                  { Envelope.headers = Headers.fromObject . KeyMap.singleton "a" $ Aeson.toJSON False,
                    Envelope.items =
                      [ Item.Item
                          { Item.headers = Headers.fromObject . KeyMap.singleton "b" $ Aeson.toJSON True,
                            Item.payload = "c"
                          }
                      ]
                  }
      actual `Hspec.shouldBe` "{\"a\":false}\n{\"b\":true}\nc\n"

    Hspec.it "works with two items" $ do
      let actual =
            Builder.toLazyByteString $
              Envelope.serialize
                Envelope.Envelope
                  { Envelope.headers = Headers.fromObject . KeyMap.singleton "a" $ Aeson.toJSON (1 :: Int),
                    Envelope.items =
                      [ Item.Item
                          { Item.headers = Headers.fromObject . KeyMap.singleton "b" $ Aeson.toJSON (2 :: Int),
                            Item.payload = "c"
                          },
                        Item.Item
                          { Item.headers = Headers.fromObject . KeyMap.singleton "d" $ Aeson.toJSON (3 :: Int),
                            Item.payload = "e"
                          }
                      ]
                  }
      actual `Hspec.shouldBe` "{\"a\":1}\n{\"b\":2}\nc\n{\"d\":3}\ne\n"
