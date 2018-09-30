module CompareBenchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigInt, negativeBigInt)
import BigInt as BI


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "compare"
        [ benchmark "one 32-digit number with itself" (\_ -> BI.compare bigInt bigInt)
        , benchmark "one positive and one negative 32-digit number" (\_ -> BI.compare bigInt negativeBigInt)
        ]
