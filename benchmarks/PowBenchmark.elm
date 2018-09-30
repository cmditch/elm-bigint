module PowBenchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigInt, negativeBigInt)
import BigInt as BI


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "pow"
        [ benchmark "positive 32-digit to the power 21" (\_ -> BI.pow bigInt (BI.fromInt 21))
        , benchmark "negative 32-digit to the power 21" (\_ -> BI.pow negativeBigInt (BI.fromInt 21))
        ]
