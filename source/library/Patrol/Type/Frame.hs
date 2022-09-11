module Patrol.Type.Frame where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
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
        [ Key.fromString "abs_path" Aeson..= absPath frame,
          Key.fromString "addr_mode" Aeson..= addrMode frame,
          Key.fromString "colno" Aeson..= colno frame,
          Key.fromString "context_line" Aeson..= contextLine frame,
          Key.fromString "filename" Aeson..= filename frame,
          Key.fromString "function" Aeson..= function frame,
          Key.fromString "image_addr" Aeson..= imageAddr frame,
          Key.fromString "in_app" Aeson..= inApp frame,
          Key.fromString "instruction_addr" Aeson..= instructionAddr frame,
          Key.fromString "lineno" Aeson..= lineno frame,
          Key.fromString "module" Aeson..= module_ frame,
          Key.fromString "package" Aeson..= package frame,
          Key.fromString "platform" Aeson..= platform frame,
          Key.fromString "post_context" Aeson..= postContext frame,
          Key.fromString "pre_context" Aeson..= preContext frame,
          Key.fromString "raw_function" Aeson..= rawFunction frame,
          Key.fromString "stack_start" Aeson..= stackStart frame,
          Key.fromString "symbol_addr" Aeson..= symbolAddr frame,
          Key.fromString "vars" Aeson..= vars frame
        ]
