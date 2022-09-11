{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.MechanismMetaSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Patrol.Type.CError as CError
import qualified Patrol.Type.MachException as MachException
import qualified Patrol.Type.MechanismMeta as MechanismMeta
import qualified Patrol.Type.NsError as NsError
import qualified Patrol.Type.PosixSignal as PosixSignal
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.MechanismMeta" $ do
  Hspec.describe "ToJSON" $ do
    let emptyMeta =
          MechanismMeta.MechanismMeta
            { MechanismMeta.errno = Nothing,
              MechanismMeta.machException = Nothing,
              MechanismMeta.nsError = Nothing,
              MechanismMeta.signal = Nothing
            }

    Hspec.it "works" $ do
      let meta = emptyMeta
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON meta `Hspec.shouldBe` json

    Hspec.it "works with a C error" $ do
      let cError =
            CError.CError
              { CError.name = Nothing,
                CError.number = Just 0
              }
          meta = emptyMeta {MechanismMeta.errno = Just cError}
          json = [Aeson.aesonQQ| { "errno": { "number": 0 } } |]
      Aeson.toJSON meta `Hspec.shouldBe` json

    Hspec.it "works with a mach exception" $ do
      let machException =
            MachException.MachException
              { MachException.code = Just 0,
                MachException.exception = Nothing,
                MachException.subcode = Nothing,
                MachException.name = Nothing
              }
          meta = emptyMeta {MechanismMeta.machException = Just machException}
          json = [Aeson.aesonQQ| { "mach_exception": { "code": 0 } } |]
      Aeson.toJSON meta `Hspec.shouldBe` json

    Hspec.it "works with an NS error" $ do
      let nsError =
            NsError.NsError
              { NsError.code = Just 0,
                NsError.domain = Nothing
              }
          meta = emptyMeta {MechanismMeta.nsError = Just nsError}
          json = [Aeson.aesonQQ| { "ns_error": { "code": 0 } } |]
      Aeson.toJSON meta `Hspec.shouldBe` json

    Hspec.it "works with a signal" $ do
      let posixSignal =
            PosixSignal.PosixSignal
              { PosixSignal.code = Just 0,
                PosixSignal.codeName = Nothing,
                PosixSignal.name = Nothing,
                PosixSignal.number = Nothing
              }
          meta = emptyMeta {MechanismMeta.signal = Just posixSignal}
          json = [Aeson.aesonQQ| { "signal": { "code": 0 } } |]
      Aeson.toJSON meta `Hspec.shouldBe` json
