{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.TransactionInfoSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.TransactionInfo as TransactionInfo
import qualified Patrol.Type.TransactionSource as TransactionSource
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.TransactionInfo" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let transactionInfo = TransactionInfo.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON transactionInfo `Hspec.shouldBe` json

    Hspec.it "works with an original" $ do
      let transactionInfo = TransactionInfo.empty {TransactionInfo.original = Just $ Text.pack "example-original"}
          json = [Aeson.aesonQQ| { "original": "example-original" } |]
      Aeson.toJSON transactionInfo `Hspec.shouldBe` json

    Hspec.it "works with a source" $ do
      let transactionInfo = TransactionInfo.empty {TransactionInfo.source = Just TransactionSource.Unknown}
          json = [Aeson.aesonQQ| { "source": "unknown" } |]
      Aeson.toJSON transactionInfo `Hspec.shouldBe` json
