module Main exposing (..)

import Html
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Html.Styled exposing (..)
import Css exposing (..)
import Array


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Verb forms" ]
        , Maybe.map
            (\{ word, radicals, form } ->
                div [] <|
                    List.foldl
                        (\letter acc ->
                            if List.member letter radicals then
                                (styled span
                                    [ color (rgb 250 0 250) ]
                                    []
                                    [ text letter ]
                                )
                                    :: acc
                            else
                                (text letter) :: acc
                        )
                        []
                        (String.split "" word |> List.reverse)
            )
            model.verb
            |> Maybe.withDefault (text "no verbs")
        , button [ onClick Previous ] [ text "previous" ]
        , button [ onClick Next ] [ text "next" ]
        ]


type alias Verb =
    { word : String
    , radicals : List String
    , form : Int
    }


verbs : Array.Array Verb
verbs =
    Array.fromList
        [ { word = "daħal", radicals = [ "d", "ħ", "l" ], form = 1 }
        , { word = "daħħal", radicals = [ "d", "ħ", "l" ], form = 2 }
        , { word = "tdaħħal", radicals = [ "d", "ħ", "l" ], form = 5 }
        ]


type alias Model =
    { verb : Maybe Verb, index : Int }


type Msg
    = Next
    | Previous


update : Msg -> Model -> Model
update msg model =
    case msg of
        Next ->
            { model | verb = Array.get (model.index + 1) verbs, index = model.index + 1 }

        Previous ->
            { model | verb = Array.get (model.index - 1) verbs, index = model.index - 1 }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { view = view >> toUnstyled
        , update = update
        , model = { verb = Array.get 0 verbs, index = 0 }
        }
