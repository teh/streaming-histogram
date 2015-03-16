module Numeric.StreamingHistogram.Internal
       ( shrink
       ) where

import qualified Data.List as L

-- Internal: Lossy compression of histogram by finding the pair of
-- values that would introduce minimum error when merged and then do a
-- weighted merge.
shrink :: [(Double, Int)] -> [(Double, Int)]
shrink m
  | length m == 0 = m
  | otherwise = shrunk'
  where
    deltas = L.zipWith (\(x, _) (y, _)-> y - x) (init m) (L.tail m)
    minDelta = L.minimum deltas
    shrunk' = shrunk (init m) (L.tail m)

    shrunk :: [(Double, Int)] -> [(Double, Int)] -> [(Double, Int)]
    shrunk [] [] = []
    shrunk ((lq, lk):ls) ((rq, rk):rs)
        | rq - lq == minDelta = ((lq * (fromIntegral lk) + rq * (fromIntegral rk)) / (fromIntegral (lk + rk)), lk + rk) : rs
        | otherwise = (lq, lk) : (shrunk ls rs)
