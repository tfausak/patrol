module Patrol.Type.Port where

import qualified Numeric.Natural as Natural

newtype Port
  = Port Natural.Natural
  deriving (Eq, Show)

fromNatural :: Natural.Natural -> Port
fromNatural = Port

toNatural :: Port -> Natural.Natural
toNatural (Port natural) = natural
