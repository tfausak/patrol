{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ExceptionSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.Exception as Exception
import qualified Patrol.Type.ExceptionValue as ExceptionValue
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Exception" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let exception = Exception.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON exception `Hspec.shouldBe` json

    Hspec.it "works with a value" $ do
      let exceptionValue =
            ExceptionValue.empty
              { ExceptionValue.type_ = Just $ Text.pack "example-type"
              }
          exception = Exception.empty {Exception.values = [exceptionValue]}
          json = [Aeson.aesonQQ| { "values": [ { "type": "example-type" } ] } |]
      Aeson.toJSON exception `Hspec.shouldBe` json
