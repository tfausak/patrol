module Patrol.Type.Item where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Patrol.Type.Event as Event
import qualified Patrol.Type.Headers as Headers

-- | <https://develop.sentry.dev/sdk/data-model/envelope-items/>
data Item = Item
  { headers :: Headers.Headers,
    payload :: ByteString.ByteString
  }
  deriving (Eq, Show)

fromEvent :: Event.Event -> Item
fromEvent event =
  let thePayload = LazyByteString.toStrict $ Aeson.encode event
   in Item
        { headers =
            Headers.fromObject $
              KeyMap.fromList
                [ (Key.fromString "type", Aeson.toJSON "event"),
                  (Key.fromString "length", Aeson.toJSON $ ByteString.length thePayload),
                  (Key.fromString "event_id", Aeson.toJSON $ Event.eventId event)
                ],
          payload = thePayload
        }

serialize :: Item -> Builder.Builder
serialize item =
  Headers.serialize (headers item)
    <> Builder.char7 '\n'
    <> Builder.byteString (payload item)
