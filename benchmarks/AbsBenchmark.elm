module AbsBenchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigInt, negativeBigInt)
import BigInt as BI


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "abs"
        [ benchmark "positive 32-digit number" (\_ -> BI.abs bigInt)
        , benchmark "negative 32-digit number" (\_ -> BI.abs negativeBigInt)
        ]
