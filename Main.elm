module Main exposing (..)

import Html
import Html.Styled.Attributes exposing (css, disabled)
import Html.Styled.Events exposing (onClick)
import Html.Styled exposing (..)
import Array
import Styles
import Maybe.Extra as Mx
import Verbs exposing (Verb, verbs, LetterType(..), toPrintable)


view : Model -> Html Msg
view model =
    case model.verb of
        Just verb ->
            viewNextVerb verb model

        Nothing ->
            div [ Styles.page ]
                [ div [ Styles.container ]
                    [ h1 [] [ text "Congrats!" ]
                    , h2 [] [ text "You've finished everything." ]
                    , button [ onClick Restart ] [ text "Start again" ]
                    ]
                ]


viewNextVerb : Verb -> Model -> Html Msg
viewNextVerb verb model =
    let
        verbView verb =
            toPrintable verb
                |> List.reverse
                |> List.foldl
                    (\letter acc ->
                        case letter of
                            Consonant char ->
                                (styled span Styles.consonant [] [ text char ])
                                    :: acc

                            Vowel char ->
                                (styled span Styles.vowel [] [ text char ])
                                    :: acc

                            Radical char ->
                                (styled span Styles.radical [] [ text char ])
                                    :: acc

                            Symbol char ->
                                (styled span Styles.marker [] [ text char ])
                                    :: acc
                    )
                    []
                |> div [ Styles.verb ]
    in
        div [ Styles.page ]
            [ div [ Styles.container ]
                [ h1 [] [ text "Verb forms" ]
                , Maybe.map verbView model.verb
                    |> Maybe.withDefault (text "no verbs")
                , text model.message
                , div [ Styles.choices ] <|
                    List.map
                        (\form ->
                            button [ onClick <| Answer form, disabled model.done ]
                                [ text <| toString form ]
                        )
                        [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
                , button [ onClick Next, disabled (not model.done) ] [ text "next" ]
                ]
            ]


type alias Model =
    { verb : Maybe Verb
    , done : Bool
    , index : Int
    , message : String
    , verbs : Array.Array Verb
    }


type Msg
    = Next
    | Answer Int
    | Restart


update : Msg -> Model -> Model
update msg model =
    case msg of
        Next ->
            initialModel (model.index + 1)

        Restart ->
            initialModel 0

        Answer form ->
            case model.verb of
                Just verb ->
                    if verb.form == form then
                        { model | message = "correct!", done = True }
                    else
                        { model | message = "wrong, it is " ++ toString verb.form, done = True }

                Nothing ->
                    model


initialModel : Int -> Model
initialModel index =
    { verbs = verbs
    , verb = Array.get index verbs
    , index = index
    , message = "choose the right answer"
    , done = False
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { view = view >> toUnstyled
        , update = update
        , model = initialModel 0
        }
