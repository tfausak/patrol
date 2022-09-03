module Patrol.Type.Response where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Typeable as Typeable
import qualified Patrol.Type.Event.Id as Id

newtype Response = Response
  { id :: Id.Id
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
