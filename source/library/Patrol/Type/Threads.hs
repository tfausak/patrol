module Patrol.Type.Threads where

import qualified Data.Aeson as Aeson
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Thread as Thread

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#threads>
newtype Threads = Threads
  { values :: [Thread.Thread]
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Threads where
  toJSON threads =
    Aeson.intoObject
      [ Aeson.pair "values" $ values threads
      ]

empty :: Threads
empty =
  Threads
    { values = []
    }
