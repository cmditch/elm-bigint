module FromStringBenchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigIntHexString, bigIntString)
import BigInt as BI


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "fromString"
        [ benchmark "a 32-digit integer" (\_ -> BI.fromIntString bigIntString)
        , benchmark "a large hex string" (\_ -> BI.fromHexString bigIntHexString)
        ]
