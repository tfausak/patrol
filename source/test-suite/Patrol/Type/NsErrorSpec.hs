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
            { NsError.code = Nothing,
              NsError.domain = Nothing
            }

    Hspec.it "works" $ do
      let nsError = emptyNsError
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON nsError `Hspec.shouldBe` json

    Hspec.it "works with a code" $ do
      let nsError = emptyNsError {NsError.code = Just 0}
          json = [Aeson.aesonQQ| { "code": 0 } |]
      Aeson.toJSON nsError `Hspec.shouldBe` json

    Hspec.it "works with a domain" $ do
      let nsError = emptyNsError {NsError.domain = Just $ Text.pack "example-domain"}
          json = [Aeson.aesonQQ| { "domain": "example-domain" } |]
      Aeson.toJSON nsError `Hspec.shouldBe` json
