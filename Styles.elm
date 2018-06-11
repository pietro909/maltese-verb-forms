module Styles exposing (..)

import Html
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Html.Styled exposing (Attribute)
import Css exposing (..)
import Css.Colors as Colors
import Array


radical =
    [ color (hex "0E9260") ]


vowel =
    []


consonant =
    [ color (rgb 0 150 250) ]


marker =
    [ color (hex "6D0310") ]


verb : Attribute msg
verb =
    css [ displayFlex, fontSize (em 2), justifyContent center, width (pct 100) ]


disableIfNot : Bool -> Attribute msg
disableIfNot can =
    if can then
        css []
    else
        css [ color Colors.silver, backgroundColor Colors.gray ]


choices : Attribute msg
choices =
    css
        [ displayFlex
        , justifyContent center
        ]


choice : Attribute msg
choice =
    css
        [ displayFlex
        , justifyContent center
        , alignItems center
        , fontSize (em 2)

        --, backgroundColor Colors.teal
        , width (em 2)
        , height (em 2)
        ]


container : Attribute msg
container =
    css
        [ displayFlex
        , flexFlow1 column
        , alignItems center
        , flex (int 1)
        ]


page : Attribute msg
page =
    css
        [ displayFlex
        , flex (int 1)
        , width (pct 100)
        , height (pct 100)
        , justifyContent center
        , alignItems center

        --, backgroundColor Colors.silver
        ]


buttonNextContainer : Attribute msg
buttonNextContainer =
    css
        [ displayFlex
        , justifyContent center
        , alignItems center
        , borderRadius (pc 50)
        , backgroundColor (hex "CF142A")
        , width (em 4)
        , height (em 4)
        , marginTop (em 2)

        --, backgroundColor (hex "0E9260")
        ]


buttonNext : Attribute msg
buttonNext =
    css
        [ color Colors.white
        , fontSize (em 1)

        --, backgroundColor (hex "0E9260")
        ]
