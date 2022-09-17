{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.PlatformSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Patrol.Type.Platform as Platform
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Platform" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works for As3" $ do
      Aeson.toJSON Platform.As3 `Hspec.shouldBe` [Aeson.aesonQQ| "as3" |]

    Hspec.it "works for C" $ do
      Aeson.toJSON Platform.C `Hspec.shouldBe` [Aeson.aesonQQ| "c" |]

    Hspec.it "works for Cfml" $ do
      Aeson.toJSON Platform.Cfml `Hspec.shouldBe` [Aeson.aesonQQ| "cfml" |]

    Hspec.it "works for Cocoa" $ do
      Aeson.toJSON Platform.Cocoa `Hspec.shouldBe` [Aeson.aesonQQ| "cocoa" |]

    Hspec.it "works for Csharp" $ do
      Aeson.toJSON Platform.Csharp `Hspec.shouldBe` [Aeson.aesonQQ| "csharp" |]

    Hspec.it "works for Elixir" $ do
      Aeson.toJSON Platform.Elixir `Hspec.shouldBe` [Aeson.aesonQQ| "elixir" |]

    Hspec.it "works for Haskell" $ do
      Aeson.toJSON Platform.Haskell `Hspec.shouldBe` [Aeson.aesonQQ| "haskell" |]

    Hspec.it "works for Go" $ do
      Aeson.toJSON Platform.Go `Hspec.shouldBe` [Aeson.aesonQQ| "go" |]

    Hspec.it "works for Groovy" $ do
      Aeson.toJSON Platform.Groovy `Hspec.shouldBe` [Aeson.aesonQQ| "groovy" |]

    Hspec.it "works for Java" $ do
      Aeson.toJSON Platform.Java `Hspec.shouldBe` [Aeson.aesonQQ| "java" |]

    Hspec.it "works for Javascript" $ do
      Aeson.toJSON Platform.Javascript `Hspec.shouldBe` [Aeson.aesonQQ| "javascript" |]

    Hspec.it "works for Native" $ do
      Aeson.toJSON Platform.Native `Hspec.shouldBe` [Aeson.aesonQQ| "native" |]

    Hspec.it "works for Node" $ do
      Aeson.toJSON Platform.Node `Hspec.shouldBe` [Aeson.aesonQQ| "node" |]

    Hspec.it "works for Objc" $ do
      Aeson.toJSON Platform.Objc `Hspec.shouldBe` [Aeson.aesonQQ| "objc" |]

    Hspec.it "works for Other" $ do
      Aeson.toJSON Platform.Other `Hspec.shouldBe` [Aeson.aesonQQ| "other" |]

    Hspec.it "works for Perl" $ do
      Aeson.toJSON Platform.Perl `Hspec.shouldBe` [Aeson.aesonQQ| "perl" |]

    Hspec.it "works for Php" $ do
      Aeson.toJSON Platform.Php `Hspec.shouldBe` [Aeson.aesonQQ| "php" |]

    Hspec.it "works for Python" $ do
      Aeson.toJSON Platform.Python `Hspec.shouldBe` [Aeson.aesonQQ| "python" |]

    Hspec.it "works for Ruby" $ do
      Aeson.toJSON Platform.Ruby `Hspec.shouldBe` [Aeson.aesonQQ| "ruby" |]
