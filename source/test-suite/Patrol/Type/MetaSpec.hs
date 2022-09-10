{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.MetaSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Patrol.Type.Meta as Meta
import qualified Patrol.Type.Signal as Signal
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Meta" $ do
  Hspec.describe "ToJSON" $ do
    let emptyMeta =
          Meta.Meta
            { Meta.signal = Nothing
            }

    Hspec.it "works" $ do
      let meta = emptyMeta
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON meta `Hspec.shouldBe` json

    Hspec.it "works with a signal" $ do
      let signal =
            Signal.Signal
              { Signal.code = Nothing,
                Signal.codeName = Nothing,
                Signal.name = Nothing,
                Signal.number = 0
              }
          meta = emptyMeta {Meta.signal = Just signal}
          json = [Aeson.aesonQQ| { "signal": { "number": 0 } } |]
      Aeson.toJSON meta `Hspec.shouldBe` json
