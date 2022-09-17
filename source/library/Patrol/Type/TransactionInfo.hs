module Patrol.Type.TransactionInfo where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Patrol.Extra.Aeson as Aeson
import qualified Patrol.Type.TransactionSource as TransactionSource

-- | <https://develop.sentry.dev/sdk/event-payloads/types/#transactioninfo>
data TransactionInfo = TransactionInfo
  { original :: Text.Text,
    source :: Maybe TransactionSource.TransactionSource
  }
  deriving (Eq, Show)

instance Aeson.ToJSON TransactionInfo where
  toJSON transactionInfo =
    Aeson.intoObject
      [ Aeson.pair "original" $ original transactionInfo,
        Aeson.pair "source" $ source transactionInfo
      ]

empty :: TransactionInfo
empty =
  TransactionInfo
    { original = Text.empty,
      source = Nothing
    }
