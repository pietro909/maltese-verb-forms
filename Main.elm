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
        , div [] <|
            List.map
                (\form ->
                    button [ onClick <| Answer form ] [ text <| toString form ]
                )
                [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
        , text model.message
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
        , { word = "ndaħal", radicals = [ "d", "ħ", "l" ], form = 7 }
        ]


type alias Model =
    { verb : Maybe Verb, index : Int, message : String }


type Msg
    = Next
    | Previous
    | Answer Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Next ->
            initialModel (model.index + 1)

        Previous ->
            initialModel (model.index - 1)

        Answer form ->
            case model.verb of
                Just verb ->
                    if verb.form == form then
                        { model | message = "correct!" }
                    else
                        { model | message = "wrong, it is " ++ toString verb.form }

                Nothing ->
                    model


initialModel : Int -> Model
initialModel index =
    { verb = Array.get index verbs, index = index, message = "choose the right answer" }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { view = view >> toUnstyled
        , update = update
        , model = initialModel 0
        }
