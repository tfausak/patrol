module Patrol.Type.Level where

import qualified Data.Aeson as Aeson

data Level
  = Debug
  | Info
  | Warning
  | Error
  | Fatal
  deriving (Eq, Show)

instance Aeson.ToJSON Level where
  toJSON level = Aeson.toJSON $ case level of
    Debug -> "debug"
    Info -> "info"
    Warning -> "warning"
    Error -> "error"
    Fatal -> "fatal"
