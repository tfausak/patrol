{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.FrameSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.Frame as Frame
import qualified Patrol.Type.Platform as Platform
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Frame" $ do
  Hspec.describe "ToJSON" $ do
    let emptyFrame =
          Frame.Frame
            { Frame.absPath = Nothing,
              Frame.addrMode = Nothing,
              Frame.colno = Nothing,
              Frame.contextLine = Nothing,
              Frame.filename = Nothing,
              Frame.function = Nothing,
              Frame.imageAddr = Nothing,
              Frame.inApp = Nothing,
              Frame.instructionAddr = Nothing,
              Frame.lineno = Nothing,
              Frame.module_ = Nothing,
              Frame.package = Nothing,
              Frame.platform = Nothing,
              Frame.postContext = [],
              Frame.preContext = [],
              Frame.rawFunction = Nothing,
              Frame.stackStart = Nothing,
              Frame.symbolAddr = Nothing,
              Frame.vars = Map.empty
            }

    Hspec.it "works" $ do
      let frame = emptyFrame
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with an abs path" $ do
      let frame = emptyFrame {Frame.absPath = Just $ Text.pack "example-abs-path"}
          json = [Aeson.aesonQQ| { "abs_path": "example-abs-path" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with an addr mode" $ do
      let frame = emptyFrame {Frame.addrMode = Just $ Text.pack "example-addr-mode"}
          json = [Aeson.aesonQQ| { "addr_mode": "example-addr-mode" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a colno" $ do
      let frame = emptyFrame {Frame.colno = Just 1}
          json = [Aeson.aesonQQ| { "colno": 1 } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a context line" $ do
      let frame = emptyFrame {Frame.contextLine = Just $ Text.pack "example-context-line"}
          json = [Aeson.aesonQQ| { "context_line": "example-context-line" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a filename" $ do
      let frame = emptyFrame {Frame.filename = Just $ Text.pack "example-filename"}
          json = [Aeson.aesonQQ| { "filename": "example-filename" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a function" $ do
      let frame = emptyFrame {Frame.function = Just $ Text.pack "example-function"}
          json = [Aeson.aesonQQ| { "function": "example-function" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with an image addr" $ do
      let frame = emptyFrame {Frame.imageAddr = Just $ Text.pack "example-image-addr"}
          json = [Aeson.aesonQQ| { "image_addr": "example-image-addr" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with an in app flag" $ do
      let frame = emptyFrame {Frame.inApp = Just True}
          json = [Aeson.aesonQQ| { "in_app": true } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with an instruction addr" $ do
      let frame = emptyFrame {Frame.instructionAddr = Just $ Text.pack "example-instruction-addr"}
          json = [Aeson.aesonQQ| { "instruction_addr": "example-instruction-addr" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a lineno" $ do
      let frame = emptyFrame {Frame.lineno = Just 1}
          json = [Aeson.aesonQQ| { "lineno": 1 } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a module" $ do
      let frame = emptyFrame {Frame.module_ = Just $ Text.pack "example-module"}
          json = [Aeson.aesonQQ| { "module": "example-module" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a package" $ do
      let frame = emptyFrame {Frame.package = Just $ Text.pack "example-package"}
          json = [Aeson.aesonQQ| { "package": "example-package" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a platform" $ do
      let frame = emptyFrame {Frame.platform = Just Platform.Other}
          json = [Aeson.aesonQQ| { "platform": "other" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with some post context" $ do
      let frame = emptyFrame {Frame.postContext = [Text.pack "example-post-context"]}
          json = [Aeson.aesonQQ| { "post_context": [ "example-post-context" ] } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with some pre context" $ do
      let frame = emptyFrame {Frame.preContext = [Text.pack "example-pre-context"]}
          json = [Aeson.aesonQQ| { "pre_context": [ "example-pre-context" ] } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a raw function" $ do
      let frame = emptyFrame {Frame.rawFunction = Just $ Text.pack "example-raw-function"}
          json = [Aeson.aesonQQ| { "raw_function": "example-raw-function" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a stack start flag" $ do
      let frame = emptyFrame {Frame.stackStart = Just True}
          json = [Aeson.aesonQQ| { "stack_start": true } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a symbol addr" $ do
      let frame = emptyFrame {Frame.symbolAddr = Just $ Text.pack "example-symbol-addr"}
          json = [Aeson.aesonQQ| { "symbol_addr": "example-symbol-addr" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with some vars" $ do
      let frame = emptyFrame {Frame.vars = Map.singleton (Text.pack "example-var") Aeson.Null}
          json = [Aeson.aesonQQ| { "vars": { "example-var": null } } |]
      Aeson.toJSON frame `Hspec.shouldBe` json
