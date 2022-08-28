module Patrol.Utility.Maybe
  ( note,
  )
where

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right
