module Patrol.Type.Response where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Typeable as Typeable
import qualified Patrol.Type.EventId as EventId

newtype Response = Response
  { id :: EventId.EventId
  }
  deriving (Eq, Show)

instance Aeson.FromJSON Response where
  parseJSON =
    let name = show $ Typeable.typeRep (Typeable.Proxy :: Typeable.Proxy Response)
     in Aeson.withObject name $ \object -> do
          theId <- object Aeson..: Key.fromString "id"
          pure
            Response
              { Patrol.Type.Response.id = theId
              }
