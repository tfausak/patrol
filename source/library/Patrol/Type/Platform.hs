module Patrol.Type.Platform where

import qualified Data.Aeson as Aeson

data Platform
  = As3
  | C
  | Cfml
  | Cocoa
  | Csharp
  | Elixir
  | Haskell
  | Go
  | Groovy
  | Java
  | Javascript
  | Native
  | Node
  | Objc
  | Other
  | Perl
  | Php
  | Python
  | Ruby
  deriving (Eq, Show)

instance Aeson.ToJSON Platform where
  toJSON platform = Aeson.toJSON $ case platform of
    As3 -> "as3"
    C -> "c"
    Cfml -> "cfml"
    Cocoa -> "cocoa"
    Csharp -> "csharp"
    Elixir -> "elixir"
    Haskell -> "haskell"
    Go -> "go"
    Groovy -> "groovy"
    Java -> "java"
    Javascript -> "javascript"
    Native -> "native"
    Node -> "node"
    Objc -> "objc"
    Other -> "other"
    Perl -> "perl"
    Php -> "php"
    Python -> "python"
    Ruby -> "ruby"
