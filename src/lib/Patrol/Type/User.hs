module Patrol.Type.User
  ( User(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Patrol.Utility.Json as Json

data User = User
  { email :: Maybe Text.Text
  , id_ :: Maybe Text.Text
  , ipAddress :: Maybe Text.Text
  , username :: Maybe Text.Text
  } deriving (Eq, Show)

instance Aeson.ToJSON User where
  toJSON request = Aeson.object $ Maybe.catMaybes
    [ Json.pair "email" <$> email request
    , Json.pair "id" <$> id_ request
    , Json.pair "ipAddress" <$> ipAddress request
    , Json.pair "username" <$> username request
    ]
