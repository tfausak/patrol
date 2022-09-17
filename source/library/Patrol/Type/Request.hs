module Patrol.Type.Request where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#request>
data Request = Request
  { cookies :: Map.Map Text.Text Text.Text,
    data_ :: Aeson.Value,
    env :: Map.Map Text.Text Aeson.Value,
    fragment :: Text.Text,
    headers :: Map.Map Text.Text Text.Text,
    inferredContentType :: Text.Text,
    method :: Text.Text,
    queryString :: Map.Map Text.Text Text.Text,
    url :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Request where
  toJSON request =
    Aeson.intoObject
      [ Aeson.pair "cookies" $ cookies request,
        Aeson.pair "data" $ data_ request,
        Aeson.pair "env" $ env request,
        Aeson.pair "fragment" $ fragment request,
        Aeson.pair "headers" $ headers request,
        Aeson.pair "inferred_content_type" $ inferredContentType request,
        Aeson.pair "method" $ method request,
        Aeson.pair "query_string" $ queryString request,
        Aeson.pair "url" $ url request
      ]

empty :: Request
empty =
  Request
    { cookies = Map.empty,
      data_ = Aeson.Null,
      env = Map.empty,
      fragment = Text.empty,
      headers = Map.empty,
      inferredContentType = Text.empty,
      method = Text.empty,
      queryString = Map.empty,
      url = Text.empty
    }
