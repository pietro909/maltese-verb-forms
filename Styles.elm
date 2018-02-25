module Styles exposing (..)

import Html
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Html.Styled exposing (Attribute)
import Css exposing (..)
import Css.Colors as Colors
import Array


radical =
    [ color (rgb 250 0 250) ]


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
    css [ displayFlex, flexFlow1 column, width (pct 50) ]


page : Attribute msg
page =
    css
        [ displayFlex
        , flex (int 1)
        , width (pct 100)
        , justifyContent center
        , backgroundColor Colors.silver
        ]
