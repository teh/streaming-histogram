{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.StreamingHistogram
-- Maintainer  :  Tom Hunger
-- Stability   :  experimental
-- Portability :  portable
--
-- A very inefficient implementation of lossy online histograms based
-- on the following paper:
--
-- * A Streaming Parallel Decision Tree Algorithm by Ben-Haim and
--   Tom-Tov:
--   <http://www.jmlr.org/papers/volume11/ben-haim10a/ben-haim10a.pdf>
--
-- __NB__ This library is very experimental and inefficient. Numerical
-- stability has not been tested (yet).

module Numeric.StreamingHistogram
       ( empty
       , update
       , Hist(..)
       , merge
       , sum
       ) where

import qualified Data.List as L
import qualified Data.Map as M
import           Numeric.StreamingHistogram.Internal (shrink)
import           Prelude hiding (sum, pi)

import Debug.Trace

data Hist where
    Hist ::
        { nBins :: Int
        , histData :: M.Map Double Int} -> Hist
    deriving (Show, Eq)

-- | Creates an empty histogram to be updated with 'update'
empty :: Int -- ^ Maximum number of values to keep
      -> Hist -- ^ The empty histogram
empty nBins' = Hist { nBins = nBins', histData = M.empty }


-- | Adds a single 'value' to the histogram.
update :: Double -- ^ Value to add
       -> Hist -- ^ Lossy histogram to update
       -> Hist -- ^ Histogram with the value incorporated
update value h =
    let updated = M.insertWith (+) value 1 (histData h)
        data' | M.size updated > nBins h = (M.fromList . shrink . M.assocs) updated
              | otherwise = updated
    in
       h { histData = data' }

-- | Merges two histograms. Returns Nothing if they don't have the same
-- number of values. TODO: Use more efficient algorithm.
merge :: Hist -- ^ Histogram a
         -> Hist -- ^ Histogram b
         -> Maybe Hist -- ^ Returns Nothing if a and b disagree on how
                       -- many values to keep.
merge ha hb =
    let union = M.unionWith (+) (histData ha) (histData hb)
        data' u
            | length u > nBins ha = data' (shrink u)
            | otherwise = u
    in
         case (nBins ha) == (nBins hb) of
            True -> Just (ha { histData = M.fromList (data' (M.assocs union)) })
            False -> Nothing

-- | Estimates how many points lie in the interval [-infinity, b].
sum :: Double -- ^ The cutoff value b
       -> Hist -- ^ A histogram
       -> Double -- ^ Estimated number of points
sum b hist =
    result
  where
    -- pad the data on both sides to make sure we always have a (p,
    -- p+1) pair even when (b < min histData) or (b > max histData).
    -- TODO (1e300 is probably not a good boundary value..).
    sorted = M.assocs (histData hist)
    result = discover 0.0 ((-1e300, 0) : sorted ++ [(1e300, 0)])

    -- Find split point for b and keep track of s so far
    discover :: Double -> [(Double, Int)] -> Double
    discover sSoFar ((pi, mi):(pj, mj):xs)
      | trace (show (sSoFar, b, (pi, mi), (pj, mj))) False = undefined
      | pi <= b && b < pj = (sTrapezoid pi mi pj mj) + sSoFar + (fromIntegral mi) / 2.0
      | otherwise = discover (sSoFar + fromIntegral mi) ((pj, mj):xs)

    -- Initial value for S (trapezoid rule)
    sTrapezoid pi mi pj mj =
        let
            mb = (fromIntegral mi) + (fromIntegral (mj - mi)) / (pj - pi) * (b - pi)
        in
            ((fromIntegral mi) + mb) / 2.0 * (b - pi) / (pj - pi)

-- TODO implement 'uniform' but I don't need it just now
