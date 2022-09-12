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
    Hspec.it "works" $ do
      let mechanismMeta = MechanismMeta.empty
          json = [Aeson.aesonQQ| {} |]
      Aeson.toJSON mechanismMeta `Hspec.shouldBe` json

    Hspec.it "works with a C error" $ do
      let cError = CError.empty {CError.number = Just 0}
          mechanismMeta = MechanismMeta.empty {MechanismMeta.errno = Just cError}
          json = [Aeson.aesonQQ| { "errno": { "number": 0 } } |]
      Aeson.toJSON mechanismMeta `Hspec.shouldBe` json

    Hspec.it "works with a mach exception" $ do
      let machException =
            MachException.empty
              { MachException.code = Just 0
              }
          mechanismMeta = MechanismMeta.empty {MechanismMeta.machException = Just machException}
          json = [Aeson.aesonQQ| { "mach_exception": { "code": 0 } } |]
      Aeson.toJSON mechanismMeta `Hspec.shouldBe` json

    Hspec.it "works with an NS error" $ do
      let nsError = NsError.empty {NsError.code = Just 0}
          mechanismMeta = MechanismMeta.empty {MechanismMeta.nsError = Just nsError}
          json = [Aeson.aesonQQ| { "ns_error": { "code": 0 } } |]
      Aeson.toJSON mechanismMeta `Hspec.shouldBe` json

    Hspec.it "works with a signal" $ do
      let posixSignal = PosixSignal.empty {PosixSignal.code = Just 0}
          mechanismMeta = MechanismMeta.empty {MechanismMeta.signal = Just posixSignal}
          json = [Aeson.aesonQQ| { "signal": { "code": 0 } } |]
      Aeson.toJSON mechanismMeta `Hspec.shouldBe` json
