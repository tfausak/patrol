module Patrol.Exception.Problem where

import qualified Control.Monad.Catch as Exception

newtype Problem
  = Problem String
  deriving (Eq, Show)

instance Exception.Exception Problem
