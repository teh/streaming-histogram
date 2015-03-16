# Streaming histograms in Haskell

This library allows to build lossy histograms online and then to
estimate how many values live in a given interval. It's based on the
paper
[A Streaming Parallel Decision Tree Algorithm](http://www.jmlr.org/papers/volume11/ben-haim10a/ben-haim10a.pdf)

There are two variants of streaming libraries: Bounded by memory and
bounded by accuracy. This library is bounded by memory. According to
the authors this library is senstive to skewed distributions but I
haven't tried yet.

*NB* This library is really inefficient and probably bad Haskell
because it is my first ever Haskell release.
