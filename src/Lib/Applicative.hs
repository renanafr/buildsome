module Lib.Applicative
  ( partitionA
  ) where

import Control.Applicative (Applicative(..), (<$>))

partitionA :: Applicative f => f Bool -> [a] -> f ([a], [a])
partitionA p = foldr step $ pure ([], [])
  where
    step x rest = combine x <$> p x <*> rest
    combine x True  (ts, fs) = (x:ts,   fs)
    combine x False (ts, fs) = (  ts, x:fs)
