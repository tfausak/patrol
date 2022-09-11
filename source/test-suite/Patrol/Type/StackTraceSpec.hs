{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.StacktraceSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.Frame as Frame
import qualified Patrol.Type.Stacktrace as Stacktrace
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Stacktrace" $ do
  Hspec.describe "ToJSON" $ do
    let emptyStacktrace =
          Stacktrace.Stacktrace
            { Stacktrace.frames = [],
              Stacktrace.registers = Map.empty
            }

    Hspec.it "works" $ do
      let stacktrace = emptyStacktrace
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON stacktrace `Hspec.shouldBe` json

    Hspec.it "works with a frame" $ do
      let frame =
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
          stacktrace = emptyStacktrace {Stacktrace.frames = [frame]}
          json = [Aeson.aesonQQ| { "frames": [ {} ] } |]
      Aeson.toJSON stacktrace `Hspec.shouldBe` json

    Hspec.it "works with a register" $ do
      let stacktrace = emptyStacktrace {Stacktrace.registers = Map.singleton (Text.pack "example-register") Aeson.Null}
          json = [Aeson.aesonQQ| { "registers": { "example-register": null } } |]
      Aeson.toJSON stacktrace `Hspec.shouldBe` json
