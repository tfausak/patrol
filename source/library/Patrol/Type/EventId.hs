module Patrol.Type.EventId where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Data.Typeable as Typeable
import qualified Data.UUID as Uuid
import qualified Data.UUID.V4 as Uuid
import qualified Patrol.Exception.Problem as Problem
import qualified Text.Printf as Printf

newtype EventId
  = EventId Uuid.UUID
  deriving (Eq, Show)

instance Aeson.FromJSON EventId where
  parseJSON =
    let name = show $ Typeable.typeRep (Typeable.Proxy :: Typeable.Proxy EventId)
     in Aeson.withText name $ maybe (fail $ "invalid " <> name) pure . fromText

instance Aeson.ToJSON EventId where
  toJSON = Aeson.toJSON . intoText

fromUuid :: Uuid.UUID -> EventId
fromUuid = EventId

intoUuid :: EventId -> Uuid.UUID
intoUuid (EventId uuid) = uuid

random :: IO.MonadIO io => io EventId
random = IO.liftIO $ fmap fromUuid Uuid.nextRandom

intoText :: EventId -> Text.Text
intoText eventId =
  let (lo, hi) = Uuid.toWords64 $ intoUuid eventId
   in Text.pack $ Printf.printf "%016x%016x" lo hi

fromText :: Exception.MonadThrow m => Text.Text -> m EventId
fromText t1 = do
  let parse :: (Exception.MonadThrow n, Integral a) => Int -> Text.Text -> n (a, Text.Text)
      parse size text = do
        let (before, after) = Text.splitAt size text
        case Text.compareLength before size of
          GT -> Exception.throwM $ Problem.Problem "impossible"
          LT -> Exception.throwM $ Problem.Problem "not enough input"
          EQ -> case Text.hexadecimal before of
            Left _ -> Exception.throwM $ Problem.Problem "invalid hexadecimal"
            Right (integral, leftover) ->
              if Text.null leftover
                then pure (integral, after)
                else Exception.throwM $ Problem.Problem "invalid hexadecimal"
  (lo, t2) <- parse 16 t1
  (hi, t3) <- parse 16 t2
  Monad.unless (Text.null t3) . Exception.throwM $ Problem.Problem "too much input"
  pure . fromUuid $ Uuid.fromWords64 lo hi
