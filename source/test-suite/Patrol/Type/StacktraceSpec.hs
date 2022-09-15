{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.StacktraceSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified GHC.Stack as Stack
import qualified Patrol.Type.Frame as Frame
import qualified Patrol.Type.Stacktrace as Stacktrace
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Stacktrace" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let stacktrace = Stacktrace.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON stacktrace `Hspec.shouldBe` json

    Hspec.it "works with a frame" $ do
      let frame = Frame.empty
          stacktrace = Stacktrace.empty {Stacktrace.frames = [frame]}
          json = [Aeson.aesonQQ| { "frames": [ {} ] } |]
      Aeson.toJSON stacktrace `Hspec.shouldBe` json

    Hspec.it "works with some registers" $ do
      let stacktrace = Stacktrace.empty {Stacktrace.registers = Map.singleton (Text.pack "example-register") Aeson.Null}
          json = [Aeson.aesonQQ| { "registers": { "example-register": null } } |]
      Aeson.toJSON stacktrace `Hspec.shouldBe` json

  Hspec.describe "fromCallStack" $ do
    Hspec.it "works with an empty call stack" $ do
      let callStack = Stack.emptyCallStack
          stacktrace = Stacktrace.empty
      Stacktrace.fromCallStack callStack `Hspec.shouldBe` stacktrace

    Hspec.it "works with a single call site" $ do
      let emptySrcLoc =
            Stack.SrcLoc
              { Stack.srcLocEndCol = 0,
                Stack.srcLocEndLine = 0,
                Stack.srcLocFile = "",
                Stack.srcLocModule = "",
                Stack.srcLocPackage = "",
                Stack.srcLocStartCol = 0,
                Stack.srcLocStartLine = 0
              }
          callStack =
            Stack.fromCallSiteList
              [ ("example-function", emptySrcLoc)
              ]
          frame =
            (Frame.fromSrcLoc emptySrcLoc)
              { Frame.function = Text.pack "example-function"
              }
          stacktrace =
            Stacktrace.empty
              { Stacktrace.frames = [frame]
              }
      Stacktrace.fromCallStack callStack `Hspec.shouldBe` stacktrace

    Hspec.it "works with two call sites" $ do
      let srcLoc1 =
            Stack.SrcLoc
              { Stack.srcLocEndCol = 1,
                Stack.srcLocEndLine = 1,
                Stack.srcLocFile = "example-file-1",
                Stack.srcLocModule = "example-module-1",
                Stack.srcLocPackage = "example-package-1",
                Stack.srcLocStartCol = 1,
                Stack.srcLocStartLine = 1
              }
          srcLoc2 =
            Stack.SrcLoc
              { Stack.srcLocEndCol = 2,
                Stack.srcLocEndLine = 2,
                Stack.srcLocFile = "example-file-2",
                Stack.srcLocModule = "example-module-2",
                Stack.srcLocPackage = "example-package-2",
                Stack.srcLocStartCol = 2,
                Stack.srcLocStartLine = 2
              }
          callStack =
            Stack.fromCallSiteList
              [ ("example-function-1", srcLoc1),
                ("example-function-2", srcLoc2)
              ]
          frame1 =
            (Frame.fromSrcLoc srcLoc1)
              { Frame.function = Text.pack "example-function-1"
              }
          frame2 =
            (Frame.fromSrcLoc srcLoc2)
              { Frame.function = Text.pack "example-function-2"
              }
          stacktrace =
            Stacktrace.empty
              { Stacktrace.frames = [frame2, frame1]
              }
      Stacktrace.fromCallStack callStack `Hspec.shouldBe` stacktrace
