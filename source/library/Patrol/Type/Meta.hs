module Patrol.Type.Meta where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Errno as Errno
import qualified Patrol.Type.MachException as MachException
import qualified Patrol.Type.NsError as NsError
import qualified Patrol.Type.Signal as Signal

data Meta = Meta
  { errno :: Maybe Errno.Errno,
    machException :: Maybe MachException.MachException,
    nsError :: Maybe NsError.NsError,
    signal :: Maybe Signal.Signal
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Meta where
  toJSON meta =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Key.fromString "errno" Aeson..= errno meta,
          Key.fromString "mach_exception" Aeson..= machException meta,
          Key.fromString "ns_error" Aeson..= nsError meta,
          Key.fromString "signal" Aeson..= signal meta
        ]
