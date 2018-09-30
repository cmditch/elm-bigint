module ToStringBenchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigInt)
import BigInt as BI


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "toString"
        [ benchmark "to int string" (\_ -> BI.toString bigInt)
        , benchmark "to hex string" (\_ -> BI.toHexString bigInt)
        ]
