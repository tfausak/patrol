module Patrol.Type.Envelope where

import qualified Data.ByteString.Builder as Builder
import qualified Patrol.Type.Headers as Headers
import qualified Patrol.Type.Item as Item

-- | <https://develop.sentry.dev/sdk/data-model/envelopes/>
data Envelope = Envelope
  { headers :: Headers.Headers,
    items :: [Item.Item]
  }
  deriving (Eq, Show)

serialize :: Envelope -> Builder.Builder
serialize envelope =
  Headers.serialize (headers envelope)
    <> Builder.char7 '\n'
    <> foldMap ((<> Builder.char7 '\n') . Item.serialize) (items envelope)
