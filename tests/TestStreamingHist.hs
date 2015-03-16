module Main where

import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import qualified Numeric.StreamingHistogram as H
import qualified Numeric.StreamingHistogram.Internal as H
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "All Tests"
    [ shrinkTests
    , updateTests
    , updateQuickCheck
    , mergeTests
    , sumTests
    ]


-- Need a small list because we're using it as input to the fold
-- function.
newtype SmallDoubleList = SmallDoubleList [Double] deriving (Eq,Show)

instance Arbitrary SmallDoubleList where
  arbitrary = sized $ \s -> do
                 n <- choose (20,s `min` 1000)
                 xs <- vectorOf n (choose (-10000.0, 10000.0))
                 return (SmallDoubleList xs)
  shrink (SmallDoubleList xs) = map SmallDoubleList (shrink xs)


updateQuickCheck = testGroup "quickcheck update"
    [ QC.testProperty "always bounded to 10" $ testOne
    ]
    where
      testOne (SmallDoubleList list) =
         let e = H.empty 10
             loop [] hist = hist
             loop (x:xs) hist = loop xs (H.update x hist)
             result = (M.size . H.histData) (loop list e)
         in
             result <= 10


shrinkTests = testGroup "shrink"
    [ testCase "unmodified" $ H.shrink [(0.0, 1)] @?= [] -- always shrink by exactly one
    , testCase "empty" $ H.shrink [] @?= []
    , testCase "actual" $ H.shrink [(0.0, 1), (1.0, 1)] @?= [(0.5, 2)]
    , testCase "leftmost" $ H.shrink [(0.0, 1), (1.0, 1), (5.0, 1)] @?= [(0.5, 2), (5.0, 1)]
    , testCase "rightmost" $ H.shrink [(0.0, 1), (4.0, 1), (5.0, 1)] @?= [(0.0, 1), (4.5, 2)]
    , testCase "middle" $ H.shrink [(0.0, 1), (1.0, 1), (3.0, 1), (3.2, 1), (4.0, 1)] @?= [(0.0,1), (1.0,1), (3.1,2), (4.0,1)]
    ]

updateTests = testGroup "update"
    [ testCase "empty" $ H.histData (H.empty 5) @?= M.empty
    , testCase "two updates" $ (M.assocs . H.histData) (H.update 1.0 (H.update 0.0 (H.empty 2))) @?= [(0.0, 1), (1.0, 1)]
    ]

mergeTests = testGroup "merge"
    [ testCase "mergeEmpty" $ H.merge (H.empty 5) (H.empty 5) @?= Just (H.empty 5)
    , testCase "mergeOnes" $ (M.assocs . H.histData . fromJust) (H.merge a b) @?= [(0.5, 2)]
    ]
  where
    a = H.update 0.0 (H.empty 1)
    b = H.update 1.0 (H.empty 1)

sumTests = testGroup "sum"
    [ testCase "sumLeft" $ H.sum (-1.0) x3 @?= 0.5
    , testCase "sumRight" $ H.sum (3.0) x3 @?= 2.5
    ]
  where
    x3 = L.foldr H.update (H.empty 3) [0.0, 1.0, 2.0]

-- | Tests from Appendix A from the Ben-Haim paper.
paperTests = testGroup "paper"
    [ testCase "update6" $ run 5 (L.take 6 testSequence) @?= [(2,1),(10,1),(17.5,2),(23,1),(36,1)]
    , testCase "update7" $ run 5 (L.take 7 testSequence) @?= [(2,1),(9.5,2),(17.5,2),(23,1),(36,1)]
    , testCase "merge3" $ (M.assocs . H.histData . fromJust) (H.merge lhist rhist) @?= [(2,1),(9.5,2),(19.33,3),(32.67,3),(45,1)]
    , testCase "sum15" $ H.sum 15 (fromJust (H.merge lhist rhist)) @?= 3.28
    ]
  where
    run nBins s = (M.assocs . H.histData) (L.foldr H.update (H.empty nBins) s)
    testSequence = [23,19,10,16,36,2,9,32,30,4] :: [Double]
    (ls, rs) = L.splitAt 7 testSequence
    lhist = L.foldr H.update (H.empty 5) ls
    rhist = L.foldr H.update (H.empty 5) rs
