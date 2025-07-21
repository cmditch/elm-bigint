module Constants exposing (hexDigitMagnitude, maxDigitMagnitude, maxDigitValue)

{-|

@docs hexDigitMagnitude, maxDigitMagnitude, maxDigitValue

-}


{-| Seven base-10 digits is the most we can have where x \* x < the JS bigInt limit.
99999999 > sqrt(MAX\_SAFE\_INTEGER) > 9999999
A slightly higher number is possible, but would require a major reworking of the string functions.
-}
maxDigitValue : Int
maxDigitValue =
    -1 + 10 ^ maxDigitMagnitude


maxDigitMagnitude : Int
maxDigitMagnitude =
    7


hexDigitMagnitude : Int
hexDigitMagnitude =
    8
