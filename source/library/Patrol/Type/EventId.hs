module Patrol.Type.EventId where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as Text
import qualified Data.Typeable as Typeable
import qualified Data.UUID as Uuid
import qualified Data.UUID.V4 as Uuid
import qualified Patrol.Exception.Problem as Problem

newtype EventId
  = EventId Uuid.UUID
  deriving (Eq, Show)

instance Aeson.FromJSON EventId where
  parseJSON =
    let name = show $ Typeable.typeRep (Typeable.Proxy :: Typeable.Proxy EventId)
     in Aeson.withText name $ maybe (fail $ "invalid " <> name) pure . fromText

instance Aeson.ToJSON EventId where
  toJSON = Aeson.toJSON . intoText

empty :: EventId
empty = fromUuid Uuid.nil

fromUuid :: Uuid.UUID -> EventId
fromUuid = EventId

intoUuid :: EventId -> Uuid.UUID
intoUuid (EventId uuid) = uuid

random :: (IO.MonadIO io) => io EventId
random = IO.liftIO $ fmap fromUuid Uuid.nextRandom

-- | Render an 'EventId' as 32 zero-padded lowercase hexadecimal digits.
--
-- __NOTE__: The output is ASCII, so 'Text.decodeLatin1' is total.
intoText :: EventId -> Text.Text
intoText eventId =
  let (lo, hi) = Uuid.toWords64 $ intoUuid eventId
   in Text.decodeLatin1
        . LazyByteString.toStrict
        . Builder.toLazyByteString
        $ Builder.word64HexFixed lo <> Builder.word64HexFixed hi

fromText :: (Catch.MonadThrow m) => Text.Text -> m EventId
fromText t1 = do
  let parse :: (Catch.MonadThrow n, Integral a) => Int -> Text.Text -> n (a, Text.Text)
      parse size text = do
        let (before, after) = Text.splitAt size text
        case Text.compareLength before size of
          GT -> Catch.throwM $ Problem.Problem "impossible"
          LT -> Catch.throwM $ Problem.Problem "not enough input"
          EQ -> case Text.hexadecimal before of
            Left _ -> Catch.throwM $ Problem.Problem "invalid hexadecimal"
            Right (integral, leftover) ->
              if Text.null leftover
                then pure (integral, after)
                else Catch.throwM $ Problem.Problem "invalid hexadecimal"
  (lo, t2) <- parse 16 t1
  (hi, t3) <- parse 16 t2
  Monad.unless (Text.null t3) . Catch.throwM $ Problem.Problem "too much input"
  pure . fromUuid $ Uuid.fromWords64 lo hi
