{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.SignalSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.Signal as Signal
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Signal" $ do
  Hspec.describe "ToJSON" $ do
    let emptySignal =
          Signal.Signal
            { Signal.code = Nothing,
              Signal.codeName = Nothing,
              Signal.name = Nothing,
              Signal.number = 0
            }

    Hspec.it "works" $ do
      let signal = emptySignal
          json = [Aeson.aesonQQ| { "number": 0 } |]
      Aeson.toJSON signal `Hspec.shouldBe` json

    Hspec.it "works with a code" $ do
      let signal = emptySignal {Signal.code = Just 1}
          json = [Aeson.aesonQQ| { "number": 0, "code": 1 } |]
      Aeson.toJSON signal `Hspec.shouldBe` json

    Hspec.it "works with a code name" $ do
      let signal = emptySignal {Signal.codeName = Just $ Text.pack "example-code-name"}
          json = [Aeson.aesonQQ| { "number": 0, "code_name": "example-code-name" } |]
      Aeson.toJSON signal `Hspec.shouldBe` json

    Hspec.it "works with a name" $ do
      let signal = emptySignal {Signal.name = Just $ Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "number": 0, "name": "example-name" } |]
      Aeson.toJSON signal `Hspec.shouldBe` json
