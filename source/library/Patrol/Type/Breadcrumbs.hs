module Patrol.Type.Breadcrumbs where

import qualified Data.Aeson as Aeson
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Breadcrumb as Breadcrumb

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#breadcrumbs>
newtype Breadcrumbs = Breadcrumbs
  { values :: [Breadcrumb.Breadcrumb]
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Breadcrumbs where
  toJSON breadcrumbs =
    Aeson.intoObject
      [ Aeson.pair "values" $ values breadcrumbs
      ]

empty :: Breadcrumbs
empty =
  Breadcrumbs
    { values = []
    }
