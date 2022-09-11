{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.MetaSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Text as Text
import qualified Patrol.Type.Errno as Errno
import qualified Patrol.Type.MachException as MachException
import qualified Patrol.Type.Meta as Meta
import qualified Patrol.Type.NsError as NsError
import qualified Patrol.Type.Signal as Signal
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Meta" $ do
  Hspec.describe "ToJSON" $ do
    let emptyMeta =
          Meta.Meta
            { Meta.errno = Nothing,
              Meta.machException = Nothing,
              Meta.nsError = Nothing,
              Meta.signal = Nothing
            }

    Hspec.it "works" $ do
      let meta = emptyMeta
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON meta `Hspec.shouldBe` json

    Hspec.it "works with an errno" $ do
      let errno =
            Errno.Errno
              { Errno.name = Nothing,
                Errno.number = 0
              }
          meta = emptyMeta {Meta.errno = Just errno}
          json = [Aeson.aesonQQ| { "errno": { "number": 0 } } |]
      Aeson.toJSON meta `Hspec.shouldBe` json

    Hspec.it "works with a mach exception" $ do
      let machException =
            MachException.MachException
              { MachException.code = 1,
                MachException.exception = 0,
                MachException.subcode = 2,
                MachException.name = Nothing
              }
          meta = emptyMeta {Meta.machException = Just machException}
          json = [Aeson.aesonQQ| { "mach_exception": { "exception": 0, "code": 1, "subcode": 2 } } |]
      Aeson.toJSON meta `Hspec.shouldBe` json

    Hspec.it "works with an NS error" $ do
      let nsError =
            NsError.NsError
              { NsError.code = 0,
                NsError.domain = Text.pack "example-domain"
              }
          meta = emptyMeta {Meta.nsError = Just nsError}
          json = [Aeson.aesonQQ| { "ns_error": { "code": 0, "domain": "example-domain" } } |]
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
