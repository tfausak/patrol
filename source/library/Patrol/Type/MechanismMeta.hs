module Patrol.Type.MechanismMeta where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
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
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "errno" Aeson..= errno mechanismMeta,
          Key.fromString "mach_exception" Aeson..= machException mechanismMeta,
          Key.fromString "ns_error" Aeson..= nsError mechanismMeta,
          Key.fromString "signal" Aeson..= signal mechanismMeta
        ]
