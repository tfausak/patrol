module Patrol.Type.TimestampSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Time as Time
import qualified Patrol.Type.Timestamp as Timestamp
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Timestamp" $ do
  Hspec.describe "fromUtcTime" $ do
    Hspec.it "works" $ do
      let utcTime = Time.UTCTime (Time.fromGregorian 2001 2 3) 0
      Timestamp.fromUtcTime utcTime `Hspec.shouldBe` Timestamp.Timestamp utcTime

  Hspec.describe "intoUtcTime" $ do
    Hspec.it "works" $ do
      let utcTime = Time.UTCTime (Time.fromGregorian 2001 2 3) 0
      Timestamp.intoUtcTime (Timestamp.fromUtcTime utcTime) `Hspec.shouldBe` utcTime

  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let timestamp = Timestamp.fromUtcTime . Time.UTCTime (Time.fromGregorian 2001 2 3) $ 4 * 60 * 60 + 5 * 60 + 6 + 0.7
          lazyByteString = Aeson.encode "2001-02-03T04:05:06.7Z"
      Aeson.encode timestamp `Hspec.shouldBe` lazyByteString
