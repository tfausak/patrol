{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.StackTraceSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.Frame as Frame
import qualified Patrol.Type.StackTrace as StackTrace
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.StackTrace" $ do
  Hspec.describe "ToJSON" $ do
    let emptyStackTrace =
          StackTrace.StackTrace
            { StackTrace.frames = [],
              StackTrace.registers = Map.empty
            }

    Hspec.it "works" $ do
      let stackTrace = emptyStackTrace
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON stackTrace `Hspec.shouldBe` json

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
          stackTrace = emptyStackTrace {StackTrace.frames = [frame]}
          json = [Aeson.aesonQQ| { "frames": [ {} ] } |]
      Aeson.toJSON stackTrace `Hspec.shouldBe` json

    Hspec.it "works with a register" $ do
      let stackTrace = emptyStackTrace {StackTrace.registers = Map.singleton (Text.pack "example-register") Aeson.Null}
          json = [Aeson.aesonQQ| { "registers": { "example-register": null } } |]
      Aeson.toJSON stackTrace `Hspec.shouldBe` json
