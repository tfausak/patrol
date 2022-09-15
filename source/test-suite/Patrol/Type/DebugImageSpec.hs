{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.DebugImageSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Type.DebugImage as DebugImage
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.DebugImage" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works with other" $ do
      let debugImage = DebugImage.Other . Map.singleton (Text.pack "example-key") $ Aeson.Bool True
          json = [Aeson.aesonQQ| { "example-key": true } |]
      Aeson.toJSON debugImage `Hspec.shouldBe` json
