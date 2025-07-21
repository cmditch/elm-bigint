module BigInt exposing
    ( BigInt
    , fromInt, fromIntString, fromHexString, toString, toHexString
    , add, sub, mul, div, modBy, divmod, pow
    , abs, negate
    , compare, gt, gte, lt, lte, max, min
    , isEven, isOdd
    )

{-| Infinite digits integers

@docs BigInt


# From/To

@docs fromInt, fromIntString, fromHexString, toString, toHexString


# Operations

@docs add, sub, mul, div, modBy, divmod, pow


# Sign

@docs abs, negate


# Comparison

@docs compare, gt, gte, lt, lte, max, min


# Misc

@docs isEven, isOdd

-}

import Basics
import Constants exposing (hexDigitMagnitude, maxDigitMagnitude, maxDigitValue)
import Hex
import List.Extra
import Maybe exposing (Maybe)
import Maybe.Extra


{-| The sign of the bigInt
-}
type Sign
    = Positive
    | Negative
    | Zero


eightHexDigits : BigInt
eightHexDigits =
    mul (fromInt 2) (fromInt 0x80000000)


signProduct : Sign -> Sign -> Sign
signProduct x y =
    if x == Zero || y == Zero then
        Zero

    else if x == y then
        Positive

    else
        Negative


signNegate : Sign -> Sign
signNegate sign_ =
    case sign_ of
        Positive ->
            Negative

        Negative ->
            Positive

        Zero ->
            Zero


signFromInt : Int -> Sign
signFromInt x =
    case Basics.compare x 0 of
        LT ->
            Negative

        GT ->
            Positive

        EQ ->
            Zero



{- From smallest to largest digit, all the digits are positive, no leading zeros -}


{-| -}
type BigInt
    = Pos Magnitude
    | Neg Magnitude
    | Zer


{-| A list of safe-size `Int`s. with smaller magnitudes closer to the head.
-}
type Magnitude
    = Magnitude (List Int)


mkBigInt : Sign -> Magnitude -> BigInt
mkBigInt s ((Magnitude digits) as mag) =
    if List.isEmpty digits then
        Zer

    else
        case s of
            Zero ->
                Zer

            Positive ->
                Pos mag

            Negative ->
                Neg mag


type BigIntNotNormalised
    = BigIntNotNormalised Sign MagnitudeNotNormalised


type MagnitudeNotNormalised
    = MagnitudeNotNormalised (List Int)


mkBigIntNotNormalised : Sign -> List Int -> BigIntNotNormalised
mkBigIntNotNormalised s digits =
    BigIntNotNormalised s (MagnitudeNotNormalised digits)


toDigits : BigInt -> List Int
toDigits bigInt =
    case bigInt of
        Zer ->
            []

        Pos (Magnitude ds) ->
            ds

        Neg (Magnitude ds) ->
            ds


baseDigit : Int
baseDigit =
    maxDigitValue + 1


{-| Makes a BigInt from an Int
-}
fromInt : Int -> BigInt
fromInt x =
    BigIntNotNormalised (signFromInt x) (MagnitudeNotNormalised [ Basics.abs x ])
        |> normalise


{-| Makes a BigInt from an integer string, positive or negative

    fromIntString "123" == Just (BigInt.Pos ...)
    fromIntString "-123" == Just (BigInt.Neg ...)
    fromIntString "" == Nothing
    fromIntString "this is not a number :P" == Nothing

-}
fromIntString : String -> Maybe BigInt
fromIntString x =
    case String.toList (String.toLower x) of
        [] ->
            Nothing

        '-' :: [] ->
            Nothing

        '-' :: xs ->
            fromString_ xs
                |> Maybe.map (mkBigInt Negative)

        '+' :: [] ->
            Nothing

        '+' :: xs ->
            fromString_ xs
                |> Maybe.map (mkBigInt Positive)

        xs ->
            fromString_ xs
                |> Maybe.map (mkBigInt Positive)


{-| Makes a BigInt from a base16 hex string, positive or negative.

    fromHexString "4b6" == Just (BigInt.Pos ...)
    fromHexString "-13d" == Just (BigInt.Neg ...)

    fromHexString "0x456" == Just (BigInt.Pos ...)
    fromHexString "-0x123" == Just (BigInt.Neg ...)

    fromHexString "R2D2" == Nothing
    fromHexString "0xC3P0" == Nothing
    fromHexString "0x" == Nothing
    fromHexString "" == Nothing

**Note:** String can be prepended with or without any combination of "0x", and "+" or "-".

-}
fromHexString : String -> Maybe BigInt
fromHexString x =
    case String.toList (String.toLower x) of
        [] ->
            Nothing

        '-' :: '0' :: 'x' :: [] ->
            Nothing

        '-' :: '0' :: 'x' :: xs ->
            fromHexString_ xs
                |> Maybe.map (mul (fromInt -1))

        '-' :: [] ->
            Nothing

        '-' :: xs ->
            fromHexString_ xs
                |> Maybe.map (mul (fromInt -1))

        '+' :: [] ->
            Nothing

        '+' :: xs ->
            fromHexString_ xs

        '0' :: 'x' :: [] ->
            Nothing

        '0' :: 'x' :: xs ->
            fromHexString_ xs

        xs ->
            fromHexString_ xs


{-| Split a number string into chunks of `maxDigitMagnitude` from smallest digits.
Turn those into integers and store as a Magnitude.
-}
fromString_ : List Char -> Maybe Magnitude
fromString_ x =
    x
        |> Maybe.Extra.traverse
            (\d ->
                let
                    r : Int
                    r =
                        Char.toCode d - Char.toCode '0'
                in
                if r >= 0 && r <= 9 then
                    Just r

                else
                    Nothing
            )
        |> Maybe.map
            (\digitList ->
                digitList
                    |> List.reverse
                    |> List.Extra.greedyGroupsOf maxDigitMagnitude
                    |> List.map
                        (\group ->
                            List.foldr (\e a -> a * 10 + e) 0 group
                        )
                    |> Magnitude
                    |> emptyZero
            )


fromHexString_ : List Char -> Maybe BigInt
fromHexString_ x =
    x
        |> Maybe.Extra.traverse
            (\d ->
                let
                    r : Int
                    r =
                        Char.toCode d - Char.toCode '0'
                in
                if r >= 0 && r <= 9 then
                    Just r

                else
                    let
                        q =
                            Char.toCode d - Char.toCode 'a' + 10
                    in
                    if q >= 10 && q < 16 then
                        Just q

                    else
                        Nothing
            )
        |> Maybe.map
            (\digitList ->
                digitList
                    |> List.reverse
                    |> List.Extra.greedyGroupsOf hexDigitMagnitude
                    |> List.map
                        (\group ->
                            List.foldr (\e a -> a * 16 + e) 0 group
                        )
                    |> List.foldr (\e s -> mul s eightHexDigits |> add (fromInt e)) zero
            )


emptyZero : Magnitude -> Magnitude
emptyZero (Magnitude xs) =
    case List.Extra.dropWhileRight ((==) 0) xs of
        [] ->
            Magnitude []

        d ->
            Magnitude d


{-| Adds two BigInts
-}
add : BigInt -> BigInt -> BigInt
add a b =
    let
        (BigIntNotNormalised _ ma) =
            toPositiveSign a

        (BigIntNotNormalised _ mb) =
            toPositiveSign b

        (MagnitudePair pairs) =
            sameSizeNotNormalized ma mb

        added =
            List.map (\( a_, b_ ) -> a_ + b_) pairs
    in
    normalise <| BigIntNotNormalised Positive (MagnitudeNotNormalised added)


{-| Changes the sign of an BigInt
-}
negate : BigInt -> BigInt
negate bigInt =
    case bigInt of
        Zer ->
            Zer

        Pos mag ->
            Neg mag

        Neg mag ->
            Pos mag


{-| Absolute value
-}
abs : BigInt -> BigInt
abs bigInt =
    case bigInt of
        Zer ->
            Zer

        Neg mag ->
            Pos mag

        i ->
            i


{-| Substracts the second BigInt from the first
-}
sub : BigInt -> BigInt -> BigInt
sub a b =
    add a (negate b)


{-| Multiplies two BigInts
-}
mul : BigInt -> BigInt -> BigInt
mul int1 int2 =
    mkBigInt
        (signProduct (sign int1) (sign int2))
        (mulMagnitudes (magnitude int1) (magnitude int2))


magnitude : BigInt -> Magnitude
magnitude bigInt =
    case bigInt of
        Zer ->
            Magnitude []

        Pos mag ->
            mag

        Neg mag ->
            mag


mulMagnitudes : Magnitude -> Magnitude -> Magnitude
mulMagnitudes (Magnitude mag1) (Magnitude mag2) =
    case mag1 of
        [] ->
            Magnitude []

        m :: [] ->
            mulSingleDigit (Magnitude mag2) m

        m :: mx ->
            let
                accum =
                    mulSingleDigit (Magnitude mag2) m

                (Magnitude rest) =
                    mulMagnitudes (Magnitude mx) (Magnitude mag2)

                bigInt =
                    add
                        (mkBigInt Positive accum)
                        (mkBigInt Positive (Magnitude (0 :: rest)))
            in
            magnitude bigInt


mulSingleDigit : Magnitude -> Int -> Magnitude
mulSingleDigit (Magnitude xs) d =
    xs
        |> List.map ((*) d)
        |> MagnitudeNotNormalised
        |> normaliseMagnitude


{-| Compares two BigInts
-}
compare : BigInt -> BigInt -> Order
compare int1 int2 =
    case ( int1, int2 ) of
        ( Pos (Magnitude mag1), Pos (Magnitude mag2) ) ->
            compareMagnitude 0 0 mag1 mag2

        ( Pos _, _ ) ->
            GT

        ( Neg (Magnitude mag1), Neg (Magnitude mag2) ) ->
            compareMagnitude 0 0 mag2 mag1

        ( Neg _, _ ) ->
            LT

        ( Zer, Pos _ ) ->
            LT

        ( Zer, Zer ) ->
            EQ

        ( Zer, Neg _ ) ->
            GT


compareMagnitude : Int -> Int -> List Int -> List Int -> Order
compareMagnitude x y xs ys =
    case ( xs, ys ) of
        ( [], [] ) ->
            Basics.compare x y

        ( [], _ ) ->
            LT

        ( _, [] ) ->
            GT

        ( x_ :: xss, y_ :: yss ) ->
            if x_ == y_ then
                compareMagnitude x y xss yss

            else
                compareMagnitude x_ y_ xss yss


{-| Less than
-}
lt : BigInt -> BigInt -> Bool
lt x y =
    compare x y == LT


{-| Greater than
-}
gt : BigInt -> BigInt -> Bool
gt x y =
    compare x y == GT


{-| Greater than or equals
-}
gte : BigInt -> BigInt -> Bool
gte x y =
    not (lt x y)


{-| Less than or equals
-}
lte : BigInt -> BigInt -> Bool
lte x y =
    not (gt x y)


{-| Returns the largest of two BigInts
-}
max : BigInt -> BigInt -> BigInt
max x y =
    if lt x y then
        y

    else
        x


{-| Returns the smallest of two BigInts
-}
min : BigInt -> BigInt -> BigInt
min x y =
    if gt x y then
        y

    else
        x


{-| Convert the BigInt to an integer string
-}
toString : BigInt -> String
toString bigInt =
    case bigInt of
        Zer ->
            "0"

        Pos mag ->
            revMagnitudeToString mag

        Neg mag ->
            "-" ++ revMagnitudeToString mag


fillZeroes : Int -> String
fillZeroes =
    String.padLeft maxDigitMagnitude '0' << String.fromInt


revMagnitudeToString : Magnitude -> String
revMagnitudeToString (Magnitude digits) =
    case List.reverse digits of
        [] ->
            "0"

        x :: xs ->
            String.concat <| String.fromInt x :: List.map fillZeroes xs


{-| Convert the BigInt to a hex string.

    toHexString (BigInt.fromInt 255) == "ff"

**Note:** "0x" will NOT be prepended to the output.

-}
toHexString : BigInt -> String
toHexString bigInt =
    case bigInt of
        Zer ->
            "0"

        Pos mag ->
            if mag == Magnitude [] then
                "0"

            else
                hexMagnitudeToString (Pos mag)

        Neg _ ->
            "-" ++ toHexString (mul (fromInt -1) bigInt)



-- Shortcut conversion to int for hex handling


bigIntToInt_ : BigInt -> Int
bigIntToInt_ bigInt =
    case bigInt of
        Zer ->
            0

        Pos (Magnitude [ a ]) ->
            a

        Pos (Magnitude [ a, b ]) ->
            b * (10 ^ maxDigitMagnitude) + a

        _ ->
            -- Note: In Elm 0.18, this last case was `Debug.crash "No suitable shortcut conversion in hexMagnitudeToString"`
            -- Using "impossible default value" instead. Should be impossible if this internal function is used correctly.
            -- Fuzz testing is very helpful.
            42


hexMagnitudeToString : BigInt -> String
hexMagnitudeToString bigInt =
    case divmod bigInt eightHexDigits of
        Nothing ->
            -- Another "impossible default value" instead of Debug.crash
            "Failure converting BigInt to hex string. Should be impossible. Open up issue on the elm-bigint repo."

        Just ( d, r ) ->
            let
                rString =
                    Hex.toString (bigIntToInt_ r)
            in
            if d == fromInt 0 then
                rString

            else
                hexMagnitudeToString d ++ String.padLeft 8 '0' rString


{-| BigInt division. Produces 0 when dividing by 0 (like (//)).
-}
div : BigInt -> BigInt -> BigInt
div num den =
    divmod num den
        |> Maybe.map Tuple.first
        |> Maybe.withDefault zero


{-| Modulus.

    modBy (BigInt.fromInt 3) (BigInt.fromInt 3)

**Note:** This function returns negative values when
the second argument is negative, unlike Basics.modBy.

-}
modBy : BigInt -> BigInt -> Maybe BigInt
modBy modulus x =
    divmod x modulus |> Maybe.map Tuple.second


{-| Square.
-}
square : BigInt -> BigInt
square num =
    mul num num


{-| Parity Check - Even.
-}
isEven : BigInt -> Bool
isEven num =
    let
        even i =
            Basics.modBy 2 i == 0
    in
    case num of
        Zer ->
            True

        Pos (Magnitude mag) ->
            even (List.head mag |> Maybe.withDefault 0)

        Neg (Magnitude mag) ->
            even (List.head mag |> Maybe.withDefault 0)


{-| Parity Check - Odd.
-}
isOdd : BigInt -> Bool
isOdd num =
    not (isEven num)


{-| Power/Exponentiation.
-}
pow : BigInt -> BigInt -> BigInt
pow base exp =
    powHelp one base exp


{-| Power helper, for sake of tail-recursion.
-}
powHelp : BigInt -> BigInt -> BigInt -> BigInt
powHelp work num exp =
    case exp of
        Zer ->
            one

        Neg _ ->
            Zer

        Pos _ ->
            if exp == one then
                mul work num

            else if isEven exp then
                powHelp work (square num) (div exp two)

            else
                powHelp (mul num work) (square num) (div (sub exp one) two)


{-| Division and modulus
-}
divmod : BigInt -> BigInt -> Maybe ( BigInt, BigInt )
divmod num den =
    if den == zero then
        Nothing

    else
        let
            cand_l =
                List.length (toDigits num) - List.length (toDigits den) + 1

            ( d, m ) =
                divMod_
                    (Basics.max 0 cand_l)
                    (abs num)
                    (abs den)
        in
        Just
            ( mkBigInt (signProduct (sign num) (sign den)) (magnitude d)
            , mkBigInt (sign num) (magnitude m)
            )


divmodDigit : BigInt -> BigInt -> BigInt -> ( BigInt, BigInt )
divmodDigit padding x y =
    divmodDigit_ (2 ^ maxDigitBits) padding x y


divmodDigit_ : Int -> BigInt -> BigInt -> BigInt -> ( BigInt, BigInt )
divmodDigit_ to_test padding num den =
    if to_test == 0 then
        ( zero, num )

    else
        let
            x =
                fromInt to_test

            candidate =
                mul (mul x den) padding

            ( newdiv, newmod ) =
                if lte candidate num then
                    ( mul x padding, sub num candidate )

                else
                    ( zero, num )

            ( restdiv, restmod ) =
                divmodDigit_ (to_test // 2) padding newmod den
        in
        ( add newdiv restdiv, restmod )


divMod_ : Int -> BigInt -> BigInt -> ( BigInt, BigInt )
divMod_ n num den =
    if n == 0 then
        divmodDigit (padDigits n) num den

    else
        let
            ( cdiv, cmod ) =
                divmodDigit (padDigits n) num den

            ( rdiv, rmod ) =
                divMod_ (n - 1) cmod den
        in
        ( add cdiv rdiv, rmod )


maxDigitBits : Int
maxDigitBits =
    maxDigitValue
        |> toFloat
        |> logBase 2
        |> ceiling


padDigits : Int -> BigInt
padDigits n =
    repeatedly (mul (fromInt baseDigit)) one n


repeatedly : (a -> a) -> a -> Int -> a
repeatedly f x n =
    List.foldl (always f) x (List.range 1 n)


sign : BigInt -> Sign
sign bigInt =
    case bigInt of
        Zer ->
            Zero

        Pos _ ->
            Positive

        Neg _ ->
            Negative


zero : BigInt
zero =
    fromInt 0


one : BigInt
one =
    fromInt 1


two : BigInt
two =
    fromInt 2


{-| We can perform operations more quickly if we don't worry about keeping things in final compressed form.
This takes a messed up number and cleans it up.
-}
normalise : BigIntNotNormalised -> BigInt
normalise (BigIntNotNormalised s digits) =
    let
        (Magnitude normalisedMag) =
            normaliseMagnitude digits
    in
    if isNegativeMagnitude normalisedMag then
        normalise (mkBigIntNotNormalised (signNegate s) (reverseMagnitude normalisedMag))

    else
        mkBigInt s (Magnitude normalisedMag)


normaliseMagnitude : MagnitudeNotNormalised -> Magnitude
normaliseMagnitude (MagnitudeNotNormalised xs) =
    Magnitude (xs |> normaliseDigitList 0 |> dropZeroes)


normaliseDigitList : Int -> List Int -> List Int
normaliseDigitList carry xs =
    case xs of
        [] ->
            if carry > baseDigit then
                normaliseDigitList 0 [ carry ]

            else
                [ carry ]

        x :: xs_ ->
            let
                ( newCarry, x_ ) =
                    normaliseDigit (x + carry)
            in
            x_ :: normaliseDigitList newCarry xs_


normaliseDigit : Int -> ( Int, Int )
normaliseDigit x =
    if x < 0 then
        normaliseDigit (x + baseDigit)
            |> Tuple.mapFirst ((+) -1)

    else
        ( x // baseDigit, remainderBy baseDigit x )


dropZeroes : List Int -> List Int
dropZeroes =
    List.Extra.dropWhileRight ((==) 0)


toPositiveSign : BigInt -> BigIntNotNormalised
toPositiveSign bigInt =
    case bigInt of
        Zer ->
            mkBigIntNotNormalised Zero []

        Neg (Magnitude digits) ->
            mkBigIntNotNormalised Positive (reverseMagnitude digits)

        Pos (Magnitude digits) ->
            mkBigIntNotNormalised Positive digits


isNegativeMagnitude : List Int -> Bool
isNegativeMagnitude digits =
    case List.Extra.last digits of
        Nothing ->
            False

        Just x ->
            x < 0


reverseMagnitude : List Int -> List Int
reverseMagnitude =
    List.map Basics.negate


{-| Magnitudes can be different sizes. This represents a pair of magnitudes with everything aligned.
-}
type MagnitudePair
    = MagnitudePair (List ( Int, Int ))


sameSizeNotNormalized : MagnitudeNotNormalised -> MagnitudeNotNormalised -> MagnitudePair
sameSizeNotNormalized (MagnitudeNotNormalised xs) (MagnitudeNotNormalised ys) =
    MagnitudePair <| sameSizeRaw xs ys


sameSizeRaw : List Int -> List Int -> List ( Int, Int )
sameSizeRaw xs ys =
    case ( xs, ys ) of
        ( [], [] ) ->
            []

        ( x :: xs_, [] ) ->
            ( x, 0 ) :: sameSizeRaw xs_ []

        ( [], y :: ys_ ) ->
            ( 0, y ) :: sameSizeRaw [] ys_

        ( x :: xs_, y :: ys_ ) ->
            ( x, y ) :: sameSizeRaw xs_ ys_
