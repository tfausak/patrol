module Patrol.Extra.ListSpec where

import qualified Data.Char as Char
import qualified Data.Function as Function
import qualified Patrol.Extra.List as List
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Extra.List" $ do
  Hspec.describe "insertAll" $ do
    Hspec.it "handles empty lists" $ do
      List.insertAll [] [] `Hspec.shouldBe` ([] :: [(Char, Bool)])

    Hspec.it "inserts into an empty list" $ do
      List.insertAll [('a', False)] [] `Hspec.shouldBe` [('a', False)]

    Hspec.it "handles an empty list of insertions" $ do
      List.insertAll [] [('a', True)] `Hspec.shouldBe` [('a', True)]

    Hspec.it "prefers the original element" $ do
      List.insertAll [('a', False)] [('a', True)] `Hspec.shouldBe` [('a', True)]

    Hspec.it "handles duplicate insertions" $ do
      List.insertAll [('a', False), ('a', True)] [] `Hspec.shouldBe` [('a', True)]

    Hspec.it "inserts into a non-empty list" $ do
      List.insertAll [('a', False)] [('b', True)] `Hspec.shouldBe` [('b', True), ('a', False)]

    Hspec.it "does not modify the original list" $ do
      List.insertAll [] [('a', False), ('a', True)] `Hspec.shouldBe` [('a', False), ('a', True)]

  Hspec.describe "insert" $ do
    Hspec.it "inserts into an empty list" $ do
      List.insert ('a', False) [] `Hspec.shouldBe` [('a', False)]

    Hspec.it "prefers the original element" $ do
      List.insert ('a', False) [('a', True)] `Hspec.shouldBe` [('a', True)]

    Hspec.it "inserts into a non-empty list" $ do
      List.insert ('a', False) [('b', True)] `Hspec.shouldBe` [('b', True), ('a', False)]

  Hspec.describe "insertBy" $ do
    Hspec.it "inserts into an empty list" $ do
      List.insertBy (==) 'a' "" `Hspec.shouldBe` "a"

    Hspec.it "does not insert a duplicate" $ do
      List.insertBy (==) 'a' "a" `Hspec.shouldBe` "a"

    Hspec.it "does not insert a duplicate earlier" $ do
      List.insertBy (==) 'a' "ab" `Hspec.shouldBe` "ab"

    Hspec.it "does not insert a duplicate later" $ do
      List.insertBy (==) 'a' "ba" `Hspec.shouldBe` "ba"

    Hspec.it "prefers the original element" $ do
      List.insertBy (Function.on (==) Char.toLower) 'a' "A" `Hspec.shouldBe` "A"

    Hspec.it "inserts into a non-empty list" $ do
      List.insertBy (==) 'a' "b" `Hspec.shouldBe` "ba"
