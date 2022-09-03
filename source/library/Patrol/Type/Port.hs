module Patrol.Type.Port where

import qualified Numeric.Natural as Natural

newtype Port
  = Port Natural.Natural
  deriving (Eq, Show)

fromNatural :: Natural.Natural -> Port
fromNatural = Port

intoNatural :: Port -> Natural.Natural
intoNatural (Port natural) = natural
