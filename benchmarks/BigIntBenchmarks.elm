module BigIntBenchmarks exposing (main)

-- Individual Benchmarks

import AbsBenchmark
import AddBenchmark
import Benchmark exposing (describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import CompareBenchmark
import DivmodBenchmark
import FromIntBenchmark
import FromStringBenchmark
import MulBenchmark
import ToStringBenchmark


main : BenchmarkProgram
main =
    program <|
        describe "BigInt"
            [ AbsBenchmark.suite
            , AddBenchmark.suite
            , CompareBenchmark.suite
            , DivmodBenchmark.suite
            , FromIntBenchmark.suite
            , FromStringBenchmark.suite
            , MulBenchmark.suite
            , ToStringBenchmark.suite
            ]
