module Main exposing (..)

import Html
import Html.Styled.Attributes exposing (css)
import Html.Styled exposing (..)
import Css exposing (..)


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
        ]


type alias Verb =
    { word : String
    , radicals : List String
    , form : Int
    }


verbs : List Verb
verbs =
    [ { word = "daħal", radicals = [ "d", "ħ", "l" ], form = 1 } ]


type alias Model =
    { verb : Maybe Verb }


type Msg
    = Fort


update : Msg -> Model -> Model
update message model =
    model


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { view = view >> toUnstyled
        , update = update
        , model = { verb = List.head verbs }
        }
