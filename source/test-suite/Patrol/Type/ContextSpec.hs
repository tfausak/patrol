{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ContextSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.Context as Context
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Context" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works with other" $ do
      let context = Context.Other . Map.singleton (Text.pack "example-key") $ Aeson.Bool True
          json = [Aeson.aesonQQ| { "example-key": true } |]
      Aeson.toJSON context `Hspec.shouldBe` json
