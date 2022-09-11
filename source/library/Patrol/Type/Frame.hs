module Patrol.Type.Frame where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.Platform as Platform

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#frame>
data Frame = Frame
  { absPath :: Maybe Text.Text,
    addrMode :: Maybe Text.Text,
    colno :: Maybe Int,
    contextLine :: Maybe Text.Text,
    filename :: Maybe Text.Text,
    function :: Maybe Text.Text,
    imageAddr :: Maybe Text.Text,
    inApp :: Maybe Bool,
    instructionAddr :: Maybe Text.Text,
    lineno :: Maybe Int,
    module_ :: Maybe Text.Text,
    package :: Maybe Text.Text,
    platform :: Maybe Platform.Platform,
    postContext :: [Text.Text],
    preContext :: [Text.Text],
    rawFunction :: Maybe Text.Text,
    stackStart :: Maybe Bool,
    symbolAddr :: Maybe Text.Text,
    vars :: Map.Map Text.Text Aeson.Value
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Frame where
  toJSON frame =
    Aeson.object $
      filter
        (not . Aeson.isEmpty . snd)
        [ Aeson.pair "abs_path" $ absPath frame,
          Aeson.pair "addr_mode" $ addrMode frame,
          Aeson.pair "colno" $ colno frame,
          Aeson.pair "context_line" $ contextLine frame,
          Aeson.pair "filename" $ filename frame,
          Aeson.pair "function" $ function frame,
          Aeson.pair "image_addr" $ imageAddr frame,
          Aeson.pair "in_app" $ inApp frame,
          Aeson.pair "instruction_addr" $ instructionAddr frame,
          Aeson.pair "lineno" $ lineno frame,
          Aeson.pair "module" $ module_ frame,
          Aeson.pair "package" $ package frame,
          Aeson.pair "platform" $ platform frame,
          Aeson.pair "post_context" $ postContext frame,
          Aeson.pair "pre_context" $ preContext frame,
          Aeson.pair "raw_function" $ rawFunction frame,
          Aeson.pair "stack_start" $ stackStart frame,
          Aeson.pair "symbol_addr" $ symbolAddr frame,
          Aeson.pair "vars" $ vars frame
        ]
