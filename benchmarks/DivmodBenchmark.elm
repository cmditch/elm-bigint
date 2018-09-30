module DivmodBenchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigInt, negativeBigInt)
import BigInt as BI


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "divmod"
        [ benchmark "one 32-digit number by 2" (\_ -> BI.divmod bigInt (BI.fromInt 2))
        , benchmark "one 32-digit number by itself" (\_ -> BI.divmod bigInt bigInt)
        ]
