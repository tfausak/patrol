module Patrol.Type.MechanismMeta where

import qualified Data.Aeson as Aeson
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.CError as CError
import qualified Patrol.Type.MachException as MachException
import qualified Patrol.Type.NsError as NsError
import qualified Patrol.Type.PosixSignal as PosixSignal

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#mechanismmeta>
data MechanismMeta = MechanismMeta
  { errno :: Maybe CError.CError,
    machException :: Maybe MachException.MachException,
    nsError :: Maybe NsError.NsError,
    signal :: Maybe PosixSignal.PosixSignal
  }
  deriving (Eq, Show)

instance Aeson.ToJSON MechanismMeta where
  toJSON mechanismMeta =
    Aeson.intoObject
      [ Aeson.pair "errno" $ errno mechanismMeta,
        Aeson.pair "mach_exception" $ machException mechanismMeta,
        Aeson.pair "ns_error" $ nsError mechanismMeta,
        Aeson.pair "signal" $ signal mechanismMeta
      ]
