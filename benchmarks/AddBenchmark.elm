module AddBenchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigInt, negativeBigInt)
import BigInt as BI


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "add"
        [ benchmark "two positive 32-digit numbers" (\_ -> BI.add bigInt bigInt)
        , benchmark "two negative 32-digit numbers" (\_ -> BI.add negativeBigInt negativeBigInt)
        ]
