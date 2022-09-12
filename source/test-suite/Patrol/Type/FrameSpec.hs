{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.FrameSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified GHC.Stack as Stack
import qualified Patrol.Type.Frame as Frame
import qualified Patrol.Type.Platform as Platform
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Frame" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let frame = Frame.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with an abs path" $ do
      let frame = Frame.empty {Frame.absPath = Just $ Text.pack "example-abs-path"}
          json = [Aeson.aesonQQ| { "abs_path": "example-abs-path" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with an addr mode" $ do
      let frame = Frame.empty {Frame.addrMode = Just $ Text.pack "example-addr-mode"}
          json = [Aeson.aesonQQ| { "addr_mode": "example-addr-mode" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a colno" $ do
      let frame = Frame.empty {Frame.colno = Just 0}
          json = [Aeson.aesonQQ| { "colno": 0 } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a context line" $ do
      let frame = Frame.empty {Frame.contextLine = Just $ Text.pack "example-context-line"}
          json = [Aeson.aesonQQ| { "context_line": "example-context-line" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a filename" $ do
      let frame = Frame.empty {Frame.filename = Just $ Text.pack "example-filename"}
          json = [Aeson.aesonQQ| { "filename": "example-filename" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a function" $ do
      let frame = Frame.empty {Frame.function = Just $ Text.pack "example-function"}
          json = [Aeson.aesonQQ| { "function": "example-function" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with an image addr" $ do
      let frame = Frame.empty {Frame.imageAddr = Just $ Text.pack "example-image-addr"}
          json = [Aeson.aesonQQ| { "image_addr": "example-image-addr" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with an in app flag" $ do
      let frame = Frame.empty {Frame.inApp = Just True}
          json = [Aeson.aesonQQ| { "in_app": true } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with an instruction addr" $ do
      let frame = Frame.empty {Frame.instructionAddr = Just $ Text.pack "example-instruction-addr"}
          json = [Aeson.aesonQQ| { "instruction_addr": "example-instruction-addr" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a lineno" $ do
      let frame = Frame.empty {Frame.lineno = Just 0}
          json = [Aeson.aesonQQ| { "lineno": 0 } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a module" $ do
      let frame = Frame.empty {Frame.module_ = Just $ Text.pack "example-module"}
          json = [Aeson.aesonQQ| { "module": "example-module" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a package" $ do
      let frame = Frame.empty {Frame.package = Just $ Text.pack "example-package"}
          json = [Aeson.aesonQQ| { "package": "example-package" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a platform" $ do
      let frame = Frame.empty {Frame.platform = Just Platform.Other}
          json = [Aeson.aesonQQ| { "platform": "other" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with some post context" $ do
      let frame = Frame.empty {Frame.postContext = [Text.pack "example-post-context"]}
          json = [Aeson.aesonQQ| { "post_context": [ "example-post-context" ] } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with some pre context" $ do
      let frame = Frame.empty {Frame.preContext = [Text.pack "example-pre-context"]}
          json = [Aeson.aesonQQ| { "pre_context": [ "example-pre-context" ] } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a raw function" $ do
      let frame = Frame.empty {Frame.rawFunction = Just $ Text.pack "example-raw-function"}
          json = [Aeson.aesonQQ| { "raw_function": "example-raw-function" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a stack start flag" $ do
      let frame = Frame.empty {Frame.stackStart = Just True}
          json = [Aeson.aesonQQ| { "stack_start": true } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with a symbol addr" $ do
      let frame = Frame.empty {Frame.symbolAddr = Just $ Text.pack "example-symbol-addr"}
          json = [Aeson.aesonQQ| { "symbol_addr": "example-symbol-addr" } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

    Hspec.it "works with some vars" $ do
      let frame = Frame.empty {Frame.vars = Map.singleton (Text.pack "example-var") Aeson.Null}
          json = [Aeson.aesonQQ| { "vars": { "example-var": null } } |]
      Aeson.toJSON frame `Hspec.shouldBe` json

  Hspec.describe "fromSrcLoc" $ do
    Hspec.it "works" $ do
      let srcLoc =
            Stack.SrcLoc
              { Stack.srcLocEndCol = 3,
                Stack.srcLocEndLine = 4,
                Stack.srcLocFile = "example-file",
                Stack.srcLocModule = "example-module",
                Stack.srcLocPackage = "example-package",
                Stack.srcLocStartCol = 1,
                Stack.srcLocStartLine = 2
              }
          frame =
            Frame.empty
              { Frame.colno = Just 1,
                Frame.filename = Just $ Text.pack "example-file",
                Frame.lineno = Just 2,
                Frame.module_ = Just $ Text.pack "example-module",
                Frame.package = Just $ Text.pack "example-package"
              }
      Frame.fromSrcLoc srcLoc `Hspec.shouldBe` frame
