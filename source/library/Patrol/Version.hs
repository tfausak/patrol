module Patrol.Version where

import qualified Data.Text as Text
import qualified Data.Version as Version
import qualified Paths_patrol as Package

string :: String
string = Version.showVersion version

text :: Text.Text
text = Text.pack string

version :: Version.Version
version = Package.version
