module Patrol.Type.Headers where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as Builder

-- | <https://develop.sentry.dev/sdk/data-model/envelopes/#headers>
newtype Headers
  = Headers Aeson.Object
  deriving (Eq, Show)

instance Aeson.FromJSON Headers where
  parseJSON = fmap fromObject . Aeson.parseJSON

instance Aeson.ToJSON Headers where
  toJSON = Aeson.Object . intoObject

fromObject :: Aeson.Object -> Headers
fromObject = Headers

intoObject :: Headers -> Aeson.Object
intoObject (Headers object) = object

empty :: Headers
empty = Headers mempty

serialize :: Headers -> Builder.Builder
serialize = Aeson.fromEncoding . Aeson.toEncoding
