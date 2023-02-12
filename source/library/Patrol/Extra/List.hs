module Patrol.Extra.List where

import qualified Data.Function as Function

insertAll :: (Eq k) => [(k, v)] -> [(k, v)] -> [(k, v)]
insertAll = flip $ foldr insert

insert :: (Eq k) => (k, v) -> [(k, v)] -> [(k, v)]
insert = insertBy $ Function.on (==) fst

insertBy :: (a -> a -> Bool) -> a -> [a] -> [a]
insertBy p x xs = case xs of
  [] -> [x]
  h : t
    | p x h -> h : t
    | otherwise -> h : insertBy p x t
