{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.ErrnoSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.Errno as Errno
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Errno" $ do
  Hspec.describe "ToJSON" $ do
    let emptyErrno =
          Errno.Errno
            { Errno.name = Nothing,
              Errno.number = Nothing
            }

    Hspec.it "works" $ do
      let errno = emptyErrno
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON errno `Hspec.shouldBe` json

    Hspec.it "works with a name" $ do
      let errno = emptyErrno {Errno.name = Just $ Text.pack "example-name"}
          json = [Aeson.aesonQQ| { "name": "example-name" } |]
      Aeson.toJSON errno `Hspec.shouldBe` json

    Hspec.it "works with a number" $ do
      let errno = emptyErrno {Errno.number = Just 0}
          json = [Aeson.aesonQQ| { "number": 0 } |]
      Aeson.toJSON errno `Hspec.shouldBe` json
