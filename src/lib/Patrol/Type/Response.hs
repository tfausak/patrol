module Patrol.Type.Response
  ( Response(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Patrol.Type.EventId as EventId
import qualified Patrol.Utility.Json as Json

newtype Response = Response
  { id :: EventId.EventId
  } deriving (Eq, Show)

instance Aeson.FromJSON Response where
  parseJSON = Aeson.withObject "Response" $ \ object -> do
    theId <- Json.required object "id"
    pure Response { Patrol.Type.Response.id = theId }
