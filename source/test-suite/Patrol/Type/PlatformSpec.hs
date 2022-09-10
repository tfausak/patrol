{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.PlatformSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Patrol.Type.Platform as Platform
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Platform" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      Aeson.toJSON Platform.As3 `Hspec.shouldBe` [Aeson.aesonQQ| "as3" |]
      Aeson.toJSON Platform.C `Hspec.shouldBe` [Aeson.aesonQQ| "c" |]
      Aeson.toJSON Platform.Cfml `Hspec.shouldBe` [Aeson.aesonQQ| "cfml" |]
      Aeson.toJSON Platform.Cocoa `Hspec.shouldBe` [Aeson.aesonQQ| "cocoa" |]
      Aeson.toJSON Platform.Csharp `Hspec.shouldBe` [Aeson.aesonQQ| "csharp" |]
      Aeson.toJSON Platform.Elixir `Hspec.shouldBe` [Aeson.aesonQQ| "elixir" |]
      Aeson.toJSON Platform.Haskell `Hspec.shouldBe` [Aeson.aesonQQ| "haskell" |]
      Aeson.toJSON Platform.Go `Hspec.shouldBe` [Aeson.aesonQQ| "go" |]
      Aeson.toJSON Platform.Groovy `Hspec.shouldBe` [Aeson.aesonQQ| "groovy" |]
      Aeson.toJSON Platform.Java `Hspec.shouldBe` [Aeson.aesonQQ| "java" |]
      Aeson.toJSON Platform.Javascript `Hspec.shouldBe` [Aeson.aesonQQ| "javascript" |]
      Aeson.toJSON Platform.Native `Hspec.shouldBe` [Aeson.aesonQQ| "native" |]
      Aeson.toJSON Platform.Node `Hspec.shouldBe` [Aeson.aesonQQ| "node" |]
      Aeson.toJSON Platform.Objc `Hspec.shouldBe` [Aeson.aesonQQ| "objc" |]
      Aeson.toJSON Platform.Other `Hspec.shouldBe` [Aeson.aesonQQ| "other" |]
      Aeson.toJSON Platform.Perl `Hspec.shouldBe` [Aeson.aesonQQ| "perl" |]
      Aeson.toJSON Platform.Php `Hspec.shouldBe` [Aeson.aesonQQ| "php" |]
      Aeson.toJSON Platform.Python `Hspec.shouldBe` [Aeson.aesonQQ| "python" |]
      Aeson.toJSON Platform.Ruby `Hspec.shouldBe` [Aeson.aesonQQ| "ruby" |]
