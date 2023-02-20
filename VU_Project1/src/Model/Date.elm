module Model.Date exposing (Date, Month(..), compare, compareMonth, full, month, monthToString, monthsBetween, monthsBetweenMonths, offsetMonths, onlyYear, view, year)

import Html exposing (Html, div, p, text)
import Model.Util exposing (chainCompare)


type Date
    = Date { year : Int, month : Maybe Month }


year : Date -> Int
year (Date d) =
    d.year


month : Date -> Maybe Month
month (Date d) =
    d.month


full : Int -> Month -> Date
full y m =
    Date { year = y, month = Just m }


onlyYear : Int -> Date
onlyYear y =
    Date { year = y, month = Nothing }


{-| Given two `Date`s it returns the number of months between the two dates as an **absolute** value.
The month fields are handled as follows:

  - If both are present (`Just`), they are included normally in the calculation
  - If both are missing (`Nothing`), the number of years between the two dates is calculated
  - Otherwise the result is undefined (`Nothing`)

```
    monthsBetween (full 2020 Jan) (full 2020 Feb) --> Just 1
    monthsBetween (full 2020 Mar) (full 2020 Jan) --> Just 2
    monthsBetween (full 2020 Jan) (full 2020 Dec) --> Just 11

    monthsBetween (full 2020 Jan) (full 2021 Feb) --> Just 13
    monthsBetween (full 2021 Jan) (full 2020 Feb) --> Just 11

    monthsBetween (onlyYear 2020) (full 2021 Jan) --> Nothing
    monthsBetween (full 2020 Jan) (onlyYear 2021) --> Nothing

    monthsBetween (full 2020 Dec) (full 2021 Jan) --> Just 1

    monthsBetween (onlyYear 2020) (onlyYear 2021) --> Just 12
    monthsBetween (onlyYear 2020) (onlyYear 2022) --> Just 24
```

-}
monthsBetween : Date -> Date -> Maybe Int
monthsBetween (Date dA) (Date dB) =
    if (dA.month == Nothing && dB.month /= Nothing) || (dA.month /= Nothing && dB.month == Nothing) then
        Nothing

    else if Basics.compare (maybeMonthToInt dA.month) (maybeMonthToInt dB.month) /= Basics.compare dA.year dB.year then
        Just
            (if
                (if dA.year > dB.year then
                    (dA.year - dB.year) * 12

                 else
                    (dB.year - dA.year) * 12
                )
                    > maybeMonthsBetweenMonths dA.month dB.month
             then
                (if dA.year > dB.year then
                    (dA.year - dB.year) * 12

                 else
                    (dB.year - dA.year) * 12
                )
                    - maybeMonthsBetweenMonths dA.month dB.month

             else
                maybeMonthsBetweenMonths dA.month dB.month
                    - (if dA.year > dB.year then
                        (dA.year - dB.year) * 12

                       else
                        (dB.year - dA.year) * 12
                      )
            )

    else
        Just
            (maybeMonthsBetweenMonths dA.month dB.month
                + (if dA.year > dB.year then
                    (dA.year - dB.year) * 12

                   else
                    (dB.year - dA.year) * 12
                  )
            )


{-| Compares two dates.
First, dates are compared by the year field. If it's equal, the month fields are used as follows:

  - If both are present (`Just`), they are compared the result is returned
  - If both are missing (`Nothing`), the dates are equal
  - Otherwise the date without month is greater

```
    Model.Date.compare (full 2020 Jan) (full 2021 Jan) --> LT
    Model.Date.compare (full 2021 Dec) (full 2021 Jan) --> GT

    Model.Date.compare (full 2020 Jan) (full 2020 Dec) --> LT
    Model.Date.compare (onlyYear 2020) (onlyYear 2021) --> LT

    Model.Date.compare (onlyYear 2020) (full 2020 Dec) --> GT
    Model.Date.compare (onlyYear 2019) (full 2020 Dec) --> LT
```

It uses chainCompare.

-}
compare : Date -> Date -> Order
compare (Date d1) (Date d2) =
    chainCompare (Basics.compare (maybeMonthToInt d1.month) (maybeMonthToInt d2.month))
        (if d1.year > d2.year then
            GT

         else if d1.year < d2.year then
            LT

         else
            EQ
        )


{-| Given a current date and the number of months, it returns a new date with the given number of months passed.
-}
offsetMonths : Int -> Date -> Date
offsetMonths months (Date d) =
    let
        addMonths =
            modBy 12 months

        addedMonths =
            d.month
                |> Maybe.map monthToInt
                |> Maybe.map ((+) addMonths)

        newMonth =
            addedMonths
                |> Maybe.map (modBy 12)
                |> Maybe.andThen intToMonth

        addYears =
            months // 12

        extraYear =
            if Maybe.withDefault 0 addedMonths >= 12 then
                1

            else
                0
    in
    Date { year = d.year + addYears + extraYear, month = newMonth }


{-| It displays the year and month.
-}
view : Date -> Html msg
view (Date d) =
    div []
        [ p [] [ text ("year: " ++ String.fromInt d.year) ]
        , p [] [ text ("month: " ++ maybeMonthToString d.month) ]
        ]


type Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec


intToMonth : Int -> Maybe Month
intToMonth idx =
    case idx of
        0 ->
            Just Jan

        1 ->
            Just Feb

        2 ->
            Just Mar

        3 ->
            Just Apr

        4 ->
            Just May

        5 ->
            Just Jun

        6 ->
            Just Jul

        7 ->
            Just Aug

        8 ->
            Just Sep

        9 ->
            Just Oct

        10 ->
            Just Nov

        11 ->
            Just Dec

        _ ->
            Nothing


monthToInt : Month -> Int
monthToInt m =
    case m of
        Jan ->
            0

        Feb ->
            1

        Mar ->
            2

        Apr ->
            3

        May ->
            4

        Jun ->
            5

        Jul ->
            6

        Aug ->
            7

        Sep ->
            8

        Oct ->
            9

        Nov ->
            10

        Dec ->
            11


{-| Convenient extension of monthToInt for Maybe values.
-}
maybeMonthToInt : Maybe Month -> Int
maybeMonthToInt m =
    case m of
        Just Jan ->
            0

        Just Feb ->
            1

        Just Mar ->
            2

        Just Apr ->
            3

        Just May ->
            4

        Just Jun ->
            5

        Just Jul ->
            6

        Just Aug ->
            7

        Just Sep ->
            8

        Just Oct ->
            9

        Just Nov ->
            10

        Just Dec ->
            11

        Nothing ->
            12


monthToString : Month -> String
monthToString m =
    case m of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


{-| Convenient extension of monthToString for Maybe values.
-}
maybeMonthToString : Maybe Month -> String
maybeMonthToString m =
    case m of
        Just Jan ->
            "January"

        Just Feb ->
            "February"

        Just Mar ->
            "March"

        Just Apr ->
            "April"

        Just May ->
            "May"

        Just Jun ->
            "June"

        Just Jul ->
            "July"

        Just Aug ->
            "August"

        Just Sep ->
            "September"

        Just Oct ->
            "October"

        Just Nov ->
            "November"

        Just Dec ->
            "December"

        Nothing ->
            "Present"


{-| Establishes an order relation between two months.
-}
compareMonth : Month -> Month -> Order
compareMonth m1 m2 =
    Basics.compare (monthToInt m1) (monthToInt m2)


{-| Absolute value of an Int.
-}
absolute : Int -> Int
absolute x =
    if x < 0 then
        -x

    else
        x


{-| Returns the number of months between two months as an **absolute** value.

    monthsBetweenMonths Jan Jan --> 0

    monthsBetweenMonths Jan Apr --> 3

    monthsBetweenMonths Apr Jan --> 3

-}
monthsBetweenMonths : Month -> Month -> Int
monthsBetweenMonths m1 m2 =
    absolute (monthToInt m1 - monthToInt m2)


{-| Convenient extension if monthsBetweenMonths for Maybe values.
-}
maybeMonthsBetweenMonths : Maybe Month -> Maybe Month -> Int
maybeMonthsBetweenMonths m1 m2 =
    absolute (maybeMonthToInt m1 - maybeMonthToInt m2)
