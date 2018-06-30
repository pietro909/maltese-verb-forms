module Styles exposing (..)

import Html
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Html.Styled exposing (Attribute)
import Css exposing (..)
import Css.Colors as Colors
import Css.Foreign as Foreign
import Array


magdaColors =
    { wheat = hex "#F7E0BC"
    , goldenrod = hex "#E7BB46"
    , steelBlue = hex "#3E7091"
    , linen = hex "#FBF4EC"
    , lightSteelBlue = hex "#A3D3D3"
    }


radical =
    [ color magdaColors.goldenrod ]


vowel =
    []


consonant =
    [ color magdaColors.lightSteelBlue ]


marker =
    []


verb : Attribute msg
verb =
    css [ displayFlex, fontSize (em 3.6), justifyContent center, width (pct 100) ]


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


instructions : Attribute msg
instructions =
    css
        []


header : Attribute msg
header =
    css
        [ padding (em 0.8)
        , color magdaColors.steelBlue
        , backgroundColor magdaColors.linen
        ]


choice : Attribute msg
choice =
    css
        [ displayFlex
        , justifyContent center
        , alignItems center
        , fontSize (em 2)
        , backgroundColor transparent
        , width (em 2)
        , height (em 2)
        , property "-webkit-appearance" "none"
        , borderWidth (em 0)
        ]


container : Attribute msg
container =
    css
        [ displayFlex
        , flexFlow1 column
        , alignItems center
        , flex (int 1)
        , justifyContent spaceBetween
        , height (pct 54)
        , paddingBottom (em 2)
        , paddingTop (em 2)
        ]


foreig =
    Foreign.global
        [ Foreign.html [ height (pct 100) ]
        , Foreign.body
            [ height (pct 100)

            --, backgroundColor magdaColors.linen
            ]
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
        , flexFlow1 column
        ]


buttonNextContainer : Attribute msg
buttonNextContainer =
    css
        [ displayFlex
        , justifyContent center
        , alignItems center
        , borderRadius (pct 50)
        , backgroundColor magdaColors.goldenrod
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
        , property "-webkit-appearance" "none"
        , borderWidth (em 0)
        , backgroundColor transparent
        ]


buttonTranslation : Attribute msg
buttonTranslation =
    css
        [ fontSize (em 1)
        , textAlign center
        , height (em 2)

        --color Colors.white
        --, property "-webkit-appearance" "none"
        , borderWidth (em 0.1)

        --, backgroundColor transparent
        , borderColor magdaColors.linen
        , borderStyle solid
        , displayFlex
        , flex (num 0.5)
        , alignItems center
        , width (pct 100)
        , justifyContent center
        ]


message : Attribute msg
message =
    css
        [ height (em 1)
        , fontFamilies [ "cursive" ]
        ]
