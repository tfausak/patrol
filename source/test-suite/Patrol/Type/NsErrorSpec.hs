{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.NsErrorSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.NsError as NsError
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.NsError" $ do
  Hspec.describe "ToJSON" $ do
    let emptyNsError =
          NsError.NsError
            { NsError.code = 0,
              NsError.domain = Text.pack "example-domain"
            }

    Hspec.it "works" $ do
      let nsError = emptyNsError
          json = [Aeson.aesonQQ| { "code": 0, "domain": "example-domain" } |]
      Aeson.toJSON nsError `Hspec.shouldBe` json
