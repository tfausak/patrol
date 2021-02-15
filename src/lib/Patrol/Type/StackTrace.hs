module Patrol.Type.StackTrace
  ( StackTrace(..)
  , fromCallStack
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NonEmpty
import qualified GHC.Stack as Stack
import qualified Patrol.Type.Frame as Frame
import qualified Patrol.Utility.Json as Json

newtype StackTrace = StackTrace
  { frames :: NonEmpty.NonEmpty Frame.Frame
  } deriving (Eq, Show)

instance Aeson.ToJSON StackTrace where
  toJSON stackTrace = Aeson.object
    [ Json.pair "frames" $ frames stackTrace
    ]

fromCallStack :: Stack.CallStack -> Maybe StackTrace
fromCallStack callStack = do
  theFrames <- NonEmpty.nonEmpty . fmap (uncurry Frame.fromSrcLoc) $ Stack.getCallStack callStack
  pure StackTrace
    { frames = theFrames
    }
