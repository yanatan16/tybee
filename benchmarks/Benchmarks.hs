module Main where

import Criterion.Main (defaultMain, bench, bgroup, Benchmark)

import Trading.Tybee ()

---------------------
-- Benchmarks
---------------------


---------------------
-- List of Benchmarks
---------------------

benches :: [Benchmark]
benches = []


---------------------
-- Main
---------------------

main :: IO ()
main = defaultMain benches