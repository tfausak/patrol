module Patrol.Type.ErrorSpec where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Patrol.Type.Error as Error
import qualified Patrol.Type.ErrorType as ErrorType
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Error" $ do
  Hspec.describe "ToJSON" $ do
    Hspec.it "works" $ do
      let error_ = Error.Error {Error.type_ = ErrorType.UnknownError, Error.value = Map.empty}
          lazyByteString = LazyByteString.fromStrict . Text.encodeUtf8 $ Text.pack "{\"type\":\"unknown_error\"}"
      Aeson.encode error_ `Hspec.shouldBe` lazyByteString

    Hspec.it "overrides the type" $ do
      let error_ = Error.Error {Error.type_ = ErrorType.UnknownError, Error.value = Map.singleton (Text.pack "type") Aeson.Null}
          lazyByteString = LazyByteString.fromStrict . Text.encodeUtf8 $ Text.pack "{\"type\":null}"
      Aeson.encode error_ `Hspec.shouldBe` lazyByteString
