module Patrol.Type.Response
  ( Response(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Utility.Json as Json

-- | <https://develop.sentry.dev/sdk/overview/#reading-the-response>
newtype Response = Response
  { id_ :: EventId.EventId
  } deriving (Eq, Show)

instance Aeson.FromJSON Response where
  parseJSON = Aeson.withObject "Response" $ \ object -> do
    id_ <- Json.required object "id"
    pure Response { id_ }
