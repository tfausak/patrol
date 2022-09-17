module Patrol.Exception.Problem where

import qualified Control.Monad.Catch as Catch

newtype Problem
  = Problem String
  deriving (Eq, Show)

instance Catch.Exception Problem
