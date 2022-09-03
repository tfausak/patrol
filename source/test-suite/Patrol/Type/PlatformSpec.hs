module Patrol.Type.PlatformSpec where

import qualified Data.Aeson as Aeson
import qualified Patrol.Type.Platform as Platform
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Platform" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      Aeson.encode Platform.As3 `Hspec.shouldBe` Aeson.encode "as3"
      Aeson.encode Platform.C `Hspec.shouldBe` Aeson.encode "c"
      Aeson.encode Platform.Cfml `Hspec.shouldBe` Aeson.encode "cfml"
      Aeson.encode Platform.Cocoa `Hspec.shouldBe` Aeson.encode "cocoa"
      Aeson.encode Platform.Csharp `Hspec.shouldBe` Aeson.encode "csharp"
      Aeson.encode Platform.Elixir `Hspec.shouldBe` Aeson.encode "elixir"
      Aeson.encode Platform.Haskell `Hspec.shouldBe` Aeson.encode "haskell"
      Aeson.encode Platform.Go `Hspec.shouldBe` Aeson.encode "go"
      Aeson.encode Platform.Groovy `Hspec.shouldBe` Aeson.encode "groovy"
      Aeson.encode Platform.Java `Hspec.shouldBe` Aeson.encode "java"
      Aeson.encode Platform.Javascript `Hspec.shouldBe` Aeson.encode "javascript"
      Aeson.encode Platform.Native `Hspec.shouldBe` Aeson.encode "native"
      Aeson.encode Platform.Node `Hspec.shouldBe` Aeson.encode "node"
      Aeson.encode Platform.Objc `Hspec.shouldBe` Aeson.encode "objc"
      Aeson.encode Platform.Other `Hspec.shouldBe` Aeson.encode "other"
      Aeson.encode Platform.Perl `Hspec.shouldBe` Aeson.encode "perl"
      Aeson.encode Platform.Php `Hspec.shouldBe` Aeson.encode "php"
      Aeson.encode Platform.Python `Hspec.shouldBe` Aeson.encode "python"
      Aeson.encode Platform.Ruby `Hspec.shouldBe` Aeson.encode "ruby"
