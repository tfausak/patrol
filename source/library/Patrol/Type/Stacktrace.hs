module Patrol.Type.Stacktrace where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified GHC.Stack as Stack
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Frame as Frame

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#stacktrace>
data Stacktrace = Stacktrace
  { frames :: [Frame.Frame],
    registers :: Map.Map Text.Text Aeson.Value
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Stacktrace where
  toJSON stacktrace =
    Aeson.intoObject
      [ Aeson.pair "frames" $ frames stacktrace,
        Aeson.pair "registers" $ registers stacktrace
      ]

empty :: Stacktrace
empty =
  Stacktrace
    { frames = [],
      registers = Map.empty
    }

fromCallStack :: Stack.CallStack -> Stacktrace
fromCallStack =
  let intoFrame string srcLoc =
        (Frame.fromSrcLoc srcLoc)
          { Frame.function = Text.pack string
          }
   in flip Stacktrace Map.empty . fmap (uncurry intoFrame) . reverse . Stack.getCallStack
