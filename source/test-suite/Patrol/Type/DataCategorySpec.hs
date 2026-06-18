{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.DataCategorySpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import Data.Foldable (for_)
import qualified Patrol.Type.DataCategory as DataCategory
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.DataCategory" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works for Attachment" $ do
      Aeson.toJSON DataCategory.Attachment `Hspec.shouldBe` [Aeson.aesonQQ| "attachment" |]

    Hspec.it "works for Default" $ do
      Aeson.toJSON DataCategory.Default `Hspec.shouldBe` [Aeson.aesonQQ| "default" |]

    Hspec.it "works for Error" $ do
      Aeson.toJSON DataCategory.Error `Hspec.shouldBe` [Aeson.aesonQQ| "error" |]

    Hspec.it "works for Feedback" $ do
      Aeson.toJSON DataCategory.Feedback `Hspec.shouldBe` [Aeson.aesonQQ| "feedback" |]

    Hspec.it "works for Internal" $ do
      Aeson.toJSON DataCategory.Internal `Hspec.shouldBe` [Aeson.aesonQQ| "internal" |]

    Hspec.it "works for LogItem" $ do
      Aeson.toJSON DataCategory.LogItem `Hspec.shouldBe` [Aeson.aesonQQ| "log_item" |]

    Hspec.it "works for Monitor" $ do
      Aeson.toJSON DataCategory.Monitor `Hspec.shouldBe` [Aeson.aesonQQ| "monitor" |]

    Hspec.it "works for Profile" $ do
      Aeson.toJSON DataCategory.Profile `Hspec.shouldBe` [Aeson.aesonQQ| "profile" |]

    Hspec.it "works for ProfileChunk" $ do
      Aeson.toJSON DataCategory.ProfileChunk `Hspec.shouldBe` [Aeson.aesonQQ| "profile_chunk" |]

    Hspec.it "works for Replay" $ do
      Aeson.toJSON DataCategory.Replay `Hspec.shouldBe` [Aeson.aesonQQ| "replay" |]

    Hspec.it "works for Security" $ do
      Aeson.toJSON DataCategory.Security `Hspec.shouldBe` [Aeson.aesonQQ| "security" |]

    Hspec.it "works for Session" $ do
      Aeson.toJSON DataCategory.Session `Hspec.shouldBe` [Aeson.aesonQQ| "session" |]

    Hspec.it "works for Span" $ do
      Aeson.toJSON DataCategory.Span `Hspec.shouldBe` [Aeson.aesonQQ| "span" |]

    Hspec.it "works for TraceMetric" $ do
      Aeson.toJSON DataCategory.TraceMetric `Hspec.shouldBe` [Aeson.aesonQQ| "trace_metric" |]

    Hspec.it "works for Transaction" $ do
      Aeson.toJSON DataCategory.Transaction `Hspec.shouldBe` [Aeson.aesonQQ| "transaction" |]

  Hspec.describe "FromJSON" $ do
    Hspec.it "fails for an unknown token" $ do
      Aeson.fromJSON [Aeson.aesonQQ| "bogus" |] `Hspec.shouldBe` (Aeson.Error "invalid DataCategory" :: Aeson.Result DataCategory.DataCategory)

    Hspec.it "round-trips every category through ToJSON" $ do
      let categories =
            [ DataCategory.Attachment,
              DataCategory.Default,
              DataCategory.Error,
              DataCategory.Feedback,
              DataCategory.Internal,
              DataCategory.LogItem,
              DataCategory.Monitor,
              DataCategory.Profile,
              DataCategory.ProfileChunk,
              DataCategory.Replay,
              DataCategory.Security,
              DataCategory.Session,
              DataCategory.Span,
              DataCategory.TraceMetric,
              DataCategory.Transaction
            ]
      for_ categories $ \c ->
        Aeson.fromJSON (Aeson.toJSON c) `Hspec.shouldBe` Aeson.Success c
