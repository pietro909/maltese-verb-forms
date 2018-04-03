module Verbs exposing (Verb, verbs, toPrintable, LetterType(..))

import Dict
import Array


type alias Verb =
    { word : String
    , radicals : List String
    , form : Int
    }


type LetterType
    = Radical String
    | Symbol String
    | Vowel String
    | Consonant String


verbs : Array.Array Verb
verbs =
    Array.fromList
        [ { word = "daħal", radicals = [ "d", "ħ", "l" ], form = 1 }
        , { word = "daħħal", radicals = [ "d", "ħ", "l" ], form = 2 }
        , { word = "tdaħħal", radicals = [ "d", "ħ", "l" ], form = 5 }
        , { word = "ndaħal", radicals = [ "d", "ħ", "l" ], form = 7 }
        , { word = "fehem", radicals = [ "f", "h", "m" ], form = 1 }
        , { word = "fiehem", radicals = [ "f", "h", "m" ], form = 3 }
        , { word = "ftehem", radicals = [ "f", "h", "m" ], form = 8 }
        , { word = "laqa'", radicals = [ "l", "q", "għ" ], form = 1 }
        , { word = "laqqa'", radicals = [ "l", "q", "għ" ], form = 2 }
        , { word = "tlaqqa'", radicals = [ "l", "q", "għ" ], form = 5 }
        , { word = "qatel", radicals = [ "q", "t", "l" ], form = 1 }
        , { word = "tqatel", radicals = [ "q", "t", "l" ], form = 6 }
        , { word = "nqatel", radicals = [ "q", "t", "l" ], form = 7 }
        , { word = "nebaħ", radicals = [ "n", "b", "ħ" ], form = 1 }
        , { word = "nebbaħ", radicals = [ "n", "b", "ħ" ], form = 2 }
        , { word = "tnebbaħ", radicals = [ "n", "b", "ħ" ], form = 5 }
        , { word = "ntebaħ", radicals = [ "n", "b", "ħ" ], form = 8 }
        , { word = "stenbaħ", radicals = [ "n", "b", "ħ" ], form = 10 }
        , { word = "bajjad", radicals = [ "b", "j", "d" ], form = 2 }
        , { word = "tbajjad", radicals = [ "b", "j", "d" ], form = 5 }
        , { word = "bjad", radicals = [ "b", "j", "d" ], form = 9 }
        ]


vowels : List String
vowels =
    [ "a", "e", "i", "o", "u" ]


symbols : List String
symbols =
    [ "'" ]


forms : Dict.Dict Int (List Int)
forms =
    Dict.fromList
        [ ( 5, [ 0 ] ) -- "t"
        , ( 6, [ 0 ] ) -- "t"
        , ( 7, [ 0 ] ) -- "n"
        , ( 8, [ 1 ] ) -- "t"
        , ( 10, [ 0, 1 ] ) --"st"
        ]


indexOf : String -> List String -> Int
indexOf a list =
    list
        |> List.indexedMap (,)
        |> List.filterMap
            (\( i, x ) ->
                if x == a then
                    Just i
                else
                    Nothing
            )
        |> List.head
        |> Maybe.withDefault -1


toPrintable : Verb -> List LetterType
toPrintable { word, radicals, form } =
    let
        indexes =
            Dict.get form forms
                |> Maybe.withDefault []
    in
        word
            |> String.split ""
            |> List.foldl
                (\current ( previous, result ) ->
                    case ( previous, current ) of
                        ( Nothing, "g" ) ->
                            -- letter "g" is cached to see if it is "għajn"
                            ( Just current, result )

                        ( Just "g", "ħ" ) ->
                            ( Nothing, "ħg" :: result )

                        ( Just "g", _ ) ->
                            ( Nothing, current :: ("g" :: result) )

                        _ ->
                            ( Nothing, current :: result )
                )
                ( Nothing, [] )
            |> Tuple.second
            |> List.reverse
            |> List.foldl
                (\letter ( index, result ) ->
                    if List.member index indexes then
                        ( index + 1, Radical letter :: result )
                    else if List.member letter vowels then
                        ( index + 1, Vowel letter :: result )
                    else if List.member letter symbols then
                        ( index + 1, Symbol letter :: result )
                    else
                        ( index + 1, Consonant letter :: result )
                )
                ( 0, [] )
            |> Tuple.second
            |> List.reverse
