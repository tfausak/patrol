module Patrol.Type.Item where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Patrol.Type.Headers as Headers

-- | <https://develop.sentry.dev/sdk/data-model/envelope-items/>
data Item = Item
  { headers :: Headers.Headers,
    payload :: ByteString.ByteString
  }
  deriving (Eq, Show)

serialize :: Item -> Builder.Builder
serialize item =
  Headers.serialize (headers item)
    <> Builder.char7 '\n'
    <> Builder.byteString (payload item)
