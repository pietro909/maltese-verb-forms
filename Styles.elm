module Styles exposing (..)

import Html
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Html.Styled exposing (Attribute)
import Css exposing (..)
import Css.Colors as Colors
import Array


radical =
    [ color (rgb 250 0 50) ]


vowel =
    [ color (rgb 50 50 50) ]


consonant =
    [ color (rgb 0 150 250) ]


marker =
    [ color (rgb 250 50 250) ]


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
    css [ displayFlex, fontSize (em 2), justifyContent center ]


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
        , backgroundColor Colors.silver
        ]
