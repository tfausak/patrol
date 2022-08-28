module Patrol.Type.Request
  ( Request (..),
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Patrol.Utility.Json as Json

data Request = Request
  { cookies :: Maybe (Map.Map Text.Text Text.Text),
    data_ :: Maybe Aeson.Value,
    env :: Maybe (Map.Map Text.Text Text.Text),
    headers :: Maybe (Map.Map Text.Text Text.Text),
    method :: Maybe Text.Text,
    queryString :: Maybe (Map.Map Text.Text Text.Text),
    url :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Request where
  toJSON request =
    Aeson.object $
      Maybe.catMaybes
        [ Json.pair "cookies" <$> cookies request,
          Json.pair "env" <$> env request,
          Json.pair "headers" <$> headers request,
          Json.pair "data" <$> data_ request,
          Json.pair "method" <$> method request,
          Json.pair "query_string" <$> queryString request,
          Json.pair "url" <$> url request
        ]
