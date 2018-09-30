module MulBenchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigInt, negativeBigInt)
import BigInt as BI


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "mul"
        [ benchmark "positive 32-digit number by itself" (\_ -> BI.mul bigInt bigInt)
        , benchmark "negative 32-digit number by positive 32-digit number" (\_ -> BI.mul negativeBigInt bigInt)
        ]
