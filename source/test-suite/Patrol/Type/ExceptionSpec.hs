{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ExceptionSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.Exception as Exception
import qualified Patrol.Type.ValueClass as ValueClass
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Exception" $ do
  Hspec.describe "ToJSON" $ do
    let emptyException =
          Exception.Exception
            { Exception.values = []
            }

    Hspec.it "works" $ do
      let exception = emptyException
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON exception `Hspec.shouldBe` json

    Hspec.it "works with a value" $ do
      let valueClass =
            ValueClass.ValueClass
              { ValueClass.mechanism = Nothing,
                ValueClass.module_ = Nothing,
                ValueClass.stacktrace = Nothing,
                ValueClass.threadId = Nothing,
                ValueClass.type_ = Just $ Text.pack "example-type",
                ValueClass.value = Nothing
              }
          exception = emptyException {Exception.values = [valueClass]}
          json = [Aeson.aesonQQ| { "values": [ { "type": "example-type" } ] } |]
      Aeson.toJSON exception `Hspec.shouldBe` json
