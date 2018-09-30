module FromIntBenchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BigInt as BI


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "fromInt"
        [ benchmark "MAX_SAFE_INTEGER" (\_ -> BI.fromInt 9007199254740991)
        ]
