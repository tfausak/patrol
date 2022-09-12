{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ExceptionsSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.ExceptionValue as ExceptionValue
import qualified Patrol.Type.Exceptions as Exceptions
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Exceptions" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let exceptions = Exceptions.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON exceptions `Hspec.shouldBe` json

    Hspec.it "works with a value" $ do
      let exceptionValue =
            ExceptionValue.empty
              { ExceptionValue.type_ = Just $ Text.pack "example-type"
              }
          exceptions = Exceptions.empty {Exceptions.values = [exceptionValue]}
          json = [Aeson.aesonQQ| { "values": [ { "type": "example-type" } ] } |]
      Aeson.toJSON exceptions `Hspec.shouldBe` json
