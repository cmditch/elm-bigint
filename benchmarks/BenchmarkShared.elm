module BenchmarkShared exposing (bigInt, bigIntHexString, bigIntString, negativeBigInt)

import BigInt as BI


bigIntString : String
bigIntString =
    "98765432100123456789987654321001234567899876543210012345678909"


bigIntHexString : String
bigIntHexString =
    "0xab5293028efd8c09b76aA37872902bbc1231def131"


bigInt : BI.BigInt
bigInt =
    BI.fromIntString bigIntString
        |> Maybe.withDefault (BI.fromInt 0)


negativeBigInt : BI.BigInt
negativeBigInt =
    BI.negate bigInt
