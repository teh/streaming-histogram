import Criterion.Main
import qualified Numeric.StreamingHistogram as H


loop [] hist = hist
loop (x:xs) hist = loop xs (H.update x hist)

a = loop [1.0..1e5] (H.empty 10)
b = loop [1.0..1e5] (H.empty 10)

main :: IO ()
main = defaultMain
    [ bgroup "update"
        [ bench "10"  $ whnf (loop [1.0..1e4]) (H.empty 10)
        , bench "100"  $ whnf (loop [1.0..1e4]) (H.empty 100)
        , bench "1000"  $ whnf (loop [1.0..1e4]) (H.empty 1000)
        ]
    , bgroup "merge"
        [ bench "1"  $ whnf (H.merge a) b
        ]
    ]
