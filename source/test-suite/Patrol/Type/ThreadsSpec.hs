{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ThreadsSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Patrol.Type.Thread as Thread
import qualified Patrol.Type.Threads as Threads
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Threads" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let threads = Threads.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON threads `Hspec.shouldBe` json

    Hspec.it "works with a value" $ do
      let thread = Thread.empty {Thread.crashed = Just True}
          threads = Threads.empty {Threads.values = [thread]}
          json = [Aeson.aesonQQ| { "values": [ { "crashed": true } ] } |]
      Aeson.toJSON threads `Hspec.shouldBe` json
