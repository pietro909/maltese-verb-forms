module Main exposing (..)

import Api
import Browser
import Html
import Html.Styled.Attributes exposing (css, disabled, name, href)
import Html.Styled.Events exposing (onClick)
import Html.Styled exposing (..)
import Http
import Array
import Styles
import Maybe.Extra as Mx
import Verbs exposing (Verb, verbs, LetterType(..), toPrintable, getWord)

import Random
import Random.List as RandomList
import RawHtml


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
            [ node "meta"
                [ name "viewport"
                -- TODO
                --, content "width=device-width, initial-scale=1.0, maximum-scale=1, user-scalable=no"
                ]
                []
            , Styles.foreig

            -- app starts here
            , header [ Styles.header ]
                [ div [ Styles.instructions ]
                    [ text "The word shown below is a verb in its "
                    , a
                        [ href "http://ablogaboutlanguages.blogspot.com/2016/05/maltese-verbs-learn-present-maltese.html"
                        ]
                        [ text "mamma" ]
                    , text ". Click the number corresponding with the verb's form."
                    ]
                , div [ Styles.instructions ]
                    [ text "The translation is provided by "
                    , a
                        [ href "http://mlrs.research.um.edu.mt/resources/gabra-api/"
                        ]
                        [ text "Ġabra API" ]
                    ]
                ]
            , div [ Styles.container ] mainNode
            ]


viewNextVerb : Verb -> Model -> List (Html Msg)
viewNextVerb verb model =
    let
        verbView v =
            toPrintable v
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

        buttonNext =
            if model.done then
                div [ Styles.buttonNextContainer ]
                    [ button [ Styles.buttonNext, onClick GetNext ] [ text "next" ] ]
            else
                div [ Styles.buttonNextContainer ]
                    [ button [ Styles.buttonNext, onClick ShowSuggestions ] [ text "hint" ] ]

        translationView =
            div [ Styles.buttonTranslation ]
                [ (case model.translation of
                    Just aTranslation ->
                        text aTranslation

                    Nothing ->
                        button [ onClick Translate ] [ text "show translation" ]
                  )
                ]

        theVerb =
            model.verb
                |> Maybe.map
                    (\v ->
                        if model.showSuggestions || model.done then
                            verbView v
                        else
                            [ text (getWord v) ]
                    )
                |> Maybe.withDefault ([ text "no verbs" ])
    in
        [ div [ Styles.verb ] theVerb
        , div [ Styles.choices ] <|
            List.map
                (\form ->
                    button
                        [ onClick <| Answer form
                        , disabled model.done
                        , Styles.choice
                        ]
                        [ text <| String.fromInt form ]
                )
                (buildButtons model.first verb.form)
        , div [ Styles.message ] [ text model.message ]
        , translationView
        , buttonNext
        ]


{-| A trivial pseudo randomizer
-}
buildButtons : Int -> Int -> List Int
buildButtons seed target =
    let
        isEven x =
            modBy 2 x == 0

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
    , translation : Maybe String
    , done : Bool
    , message : String
    , first : Int
    , verbsToSee : List Verb
    , showSuggestions : Bool
    }


type Msg
    = Next ( Int, Maybe Verb, List Verb )
    | GetNext
    | Answer Int
    | Restart
    | ShowSuggestions
    | Translate
    | Translation (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Translation (Ok result) ->
            let
                newModel =
                    { model | translation = Just result }
            in
                ( newModel, Cmd.none )

        Translation (Err e) ->
            let
                newModel =
                    { model | translation = Just "sorry, couldn't translate this verb :-(" }
            in
                ( newModel, Cmd.none )

        Translate ->
            model.verb
                |> Maybe.map
                    (\{ word, radicals, form } ->
                        let
                            cmd =
                                Api.getVerb Translation word
                        in
                            ( model, cmd )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        Next ( first, maybeVerb, verbsToSee ) ->
            let
                newModel =
                    { initialModel | verbsToSee = verbsToSee, verb = maybeVerb, first = first }
            in
                ( newModel, Cmd.none )

        GetNext ->
            ( model, generateNextData model.verbsToSee Next )

        ShowSuggestions ->
            ( { model | showSuggestions = True }, Cmd.none )

        Restart ->
            init

        Answer form ->
            case model.verb of
                Just verb ->
                    if verb.form == form then
                        ( { model | message = "correct!", done = True }, Cmd.none )
                    else
                        ( { model | message = "wrong, it is " ++ String.fromInt verb.form, done = True }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


generateNextData : List Verb -> (( Int, Maybe Verb, List Verb ) -> Msg) -> Cmd Msg
generateNextData inputVerbs msg =
    Random.generate msg <|
        Random.map2
            (\index ( maybeVerb, outputVerbs ) -> ( index, maybeVerb, outputVerbs ))
            (Random.int 1 10)
            (RandomList.choose inputVerbs)


initialModel : Model
initialModel =
    { verbsToSee = verbs
    , verb = Nothing
    , first = 5
    , message = ""
    , done = False
    , showSuggestions = False
    , translation = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, generateNextData initialModel.verbsToSee Next )


main : Program (Maybe String) Model Msg
main =
    Browser.document
        { init = \_ -> init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = \model -> {
                title = "Maltese verbs"
                , body = [view model |> toUnstyled ]
        }
        }
