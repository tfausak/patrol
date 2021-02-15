module Patrol.Type.Frame
  ( Frame(..)
  , fromSrcLoc
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified GHC.Stack as Stack
import qualified Patrol.Utility.Json as Json

data Frame = Frame
  { colno :: Maybe Int
  , filename :: Maybe Text.Text
  , function :: Text.Text
  , lineno :: Maybe Int
  , module_ :: Maybe Text.Text
  , package :: Maybe Text.Text
  } deriving (Eq, Show)

instance Aeson.ToJSON Frame where
  toJSON frame = Aeson.object $ Maybe.catMaybes
    [ Json.pair "colno" <$> colno frame
    , Json.pair "filename" <$> filename frame
    , Just . Json.pair "function" $ function frame
    , Json.pair "lineno" <$> lineno frame
    , Json.pair "module" <$> module_ frame
    , Json.pair "package" <$> package frame
    ]

fromSrcLoc :: String -> Stack.SrcLoc -> Frame
fromSrcLoc theFunction srcLoc = Frame
  { colno = Just $ Stack.srcLocStartCol srcLoc
  , filename = Just . Text.pack $ Stack.srcLocFile srcLoc
  , function = Text.pack theFunction
  , lineno = Just $ Stack.srcLocStartLine srcLoc
  , module_ = Just . Text.pack $ Stack.srcLocModule srcLoc
  , package = Just . Text.pack $ Stack.srcLocPackage srcLoc
  }
