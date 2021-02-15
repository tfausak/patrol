module Patrol.Type.Level
  ( Level(..)
  ) where

import qualified Data.Aeson as Aeson

data Level
  = Fatal
  | Error
  | Warning
  | Info
  | Debug
  deriving (Eq, Show)

instance Aeson.ToJSON Level where
  toJSON level = Aeson.toJSON $ case level of
    Fatal -> "fatal"
    Error -> "error"
    Warning -> "warning"
    Info -> "info"
    Debug -> "debug"
