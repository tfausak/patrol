{-# LANGUAGE OverloadedStrings #-}

module Patrol.Type.HeadersSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Builder as Builder
import qualified Patrol.Type.Headers as Headers
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Headers" $ do
  Hspec.describe "serialize" $ do
    Hspec.it "works with an empty object" $ do
      let actual = Builder.toLazyByteString $ Headers.serialize Headers.empty
      actual `Hspec.shouldBe` "{}"

    Hspec.it "works with a singleton object" $ do
      let actual =
            Builder.toLazyByteString
              . Headers.serialize
              . Headers.fromObject
              $ KeyMap.singleton "k" Aeson.Null
      actual `Hspec.shouldBe` "{\"k\":null}"

    Hspec.it "works with a non-empty object" $ do
      let actual =
            Builder.toLazyByteString
              . Headers.serialize
              . Headers.fromObject
              $ KeyMap.fromList [("a", Aeson.toJSON False), ("b", Aeson.toJSON True)]
      actual `Hspec.shouldBe` "{\"a\":false,\"b\":true}"

    Hspec.it "escapes newlines" $ do
      let actual =
            Builder.toLazyByteString
              . Headers.serialize
              . Headers.fromObject
              $ KeyMap.singleton "k" "\n"
      actual `Hspec.shouldBe` "{\"k\":\"\\n\"}"
