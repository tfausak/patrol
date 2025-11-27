module Patrol.Type.Items where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString as ByteString
import qualified Patrol.Type.Item as Item

-- | The items contained in an 'Patrol.Type.Envelope.Envelope'.
--
-- This is typically a list of 'Patrol.Type.Item.Item', but can also be a
-- binary blob of 'ByteString.ByteString'.
data Items
  = EnvelopeItems [Item.Item]
  | Raw ByteString.ByteString
  deriving (Eq, Show)

serialize :: Items -> Builder.Builder
serialize items = case items of
  Raw buf -> Builder.byteString buf
  EnvelopeItems envelopeItems ->
    foldMap ((<> Builder.char7 '\n') . Item.serialize) envelopeItems
