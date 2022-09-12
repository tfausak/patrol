module Patrol.Type.BreadcrumbType where

import qualified Data.Aeson as Aeson

data BreadcrumbType
  = Default
  | Http
  | Navigation
  deriving (Eq, Show)

instance Aeson.ToJSON BreadcrumbType where
  toJSON breadcrumbType = Aeson.toJSON $ case breadcrumbType of
    Default -> "default"
    Http -> "http"
    Navigation -> "navigation"
