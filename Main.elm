module Main exposing (..)

import Html
import Html.Styled.Attributes exposing (css, disabled, name, content)
import Html.Styled.Events exposing (onClick)
import Html.Styled exposing (..)
import Array
import Styles
import Maybe.Extra as Mx
import Verbs exposing (Verb, verbs, LetterType(..), toPrintable)
import Random


view : Model -> Html Msg
view model =
    let
        mainNode =
            case model.verb of
                Just verb ->
                    viewNextVerb verb model

                Nothing ->
                    [ h1 [] [ text "Congrats!" ]
                    , h2 [] [ text "You've finished everything." ]
                    , button [ onClick Restart ] [ text "Start again" ]
                    ]
    in
        div [ Styles.page ]
            [ node "meta" [ name "viewport", content "width=device-width, initial-scale=1.0, maximum-scale=1, user-scalable=no" ] []
            , div [ Styles.container ] mainNode
            ]


viewNextVerb : Verb -> Model -> List (Html Msg)
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

        buttonNext =
            if model.done then
                div [ Styles.buttonNextContainer ]
                    [ button [ Styles.buttonNext, onClick GetNext, disabled (not model.done) ] [ text "next" ] ]
            else
                div [ Styles.buttonNextContainer ]
                    [ button [ Styles.buttonNext, onClick GetNext, disabled (not model.done) ] [ text "?" ] ]
    in
        [ Maybe.map verbView model.verb
            |> Maybe.withDefault (text "no verbs")

        --, text model.message
        , div [ Styles.choices ] <|
            List.map
                (\form ->
                    button
                        [ onClick <| Answer form
                        , disabled model.done
                        , Styles.choice
                        ]
                        [ text <| toString form ]
                )
                (buildButtons model.first verb.form)
        , buttonNext
        ]


{-| A trivial pseudo randomizer
-}
buildButtons seed target =
    let
        isEven x =
            rem x 2 == 0

        numbersA =
            [ 1, 2, 3, 4, 5 ]

        numbersB =
            [ 6, 7, 8, 9, 10 ]

        mix =
            List.foldl
                (\n ( front, back ) ->
                    if n == seed then
                        ( [], front ++ n :: back )
                    else if isEven n then
                        ( n :: front, back )
                    else
                        ( back, List.append front [ n ] )
                )
                ( [], [] )
                >> (\( f, b ) ->
                        if isEven target then
                            List.reverse b ++ f
                        else
                            List.reverse (b ++ f)
                   )

        start =
            if isEven seed then
                mix (mix numbersA ++ List.reverse numbersB)
            else
                mix numbersB ++ mix (List.reverse numbersA)
    in
        List.foldl
            (\n accumulator ->
                if List.member n accumulator then
                    mix accumulator
                else if List.length accumulator == 4 then
                    accumulator
                else if isEven n then
                    n :: accumulator
                else
                    List.append accumulator [ n ]
            )
            [ target ]
            start
            |> mix


type alias Model =
    { verb : Maybe Verb
    , done : Bool
    , index : Int
    , message : String
    , first : Int
    , verbs : Array.Array Verb
    }


type Msg
    = Next Int
    | GetNext
    | Answer Int
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next first ->
            ( initialModel first (model.index + 1), Cmd.none )

        GetNext ->
            ( model, generateNumber )

        Restart ->
            ( initialModel 5 0, Cmd.none )

        Answer form ->
            case model.verb of
                Just verb ->
                    if verb.form == form then
                        ( { model | message = "correct!", done = True }, Cmd.none )
                    else
                        ( { model | message = "wrong, it is " ++ toString verb.form, done = True }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


generateNumber =
    Random.generate Next (Random.int 1 10)


initialModel : Int -> Int -> Model
initialModel first index =
    { verbs = verbs
    , verb = Array.get index verbs
    , first = first
    , index = index
    , message = "choose the right answer"
    , done = False
    }


main : Program Never Model Msg
main =
    Html.program
        { view = view >> toUnstyled
        , update = update
        , init = ( initialModel 5 0, generateNumber )
        , subscriptions = \_ -> Sub.none
        }
