module Showdocumentview exposing (view)

import Model exposing (..)
import Msg exposing (..)
import Document exposing (Document)
import IconSet exposing (..)

import Html exposing (Html, text, h1, br)
import Material.Options exposing (Property, css, cs, div, span, onClick, center)
import Material.Button as Button
import Material.Icon as Icon
import Material.Elevation as Elevation

view : Model -> Document -> Property c Msg -> Int -> Html Msg
view model document flex slotId =
    div
        [ cs "slot"
        , flex
        , css "padding" "0px 10px"
        -- , css "margin" "3px 0"
        , css "box-shadow" "0 0 10px rgba(0, 0, 0, 0.80)"
        , Elevation.e0
        , primaryColor
        , css "display" "inline-flex"
        ]
        [ div
            [ css "height" "45px"
            , center
            ]
            [ iconDocument model.mdl [ css "margin" "5px"]
            , span
                [ css "width" "calc(100% - 64px)"
                , css "text-align" "left"
                ]
                [ text document.title]
            , Button.render Mdl [slotId] model.mdl
                [ cs "slot__close_button"
                , Button.fab
                , Button.minifab
                , Button.raised
                , Button.ripple
                , onClick (DeleteSlot slotId)
                ]
                [ Icon.i "close" ]
            ]
        , document2DocumentView document
        ]

document2DocumentView : Document -> Html Msg
document2DocumentView document =
    div [ css "margin" "0px 6px"
        , css "height" "calc(100% - 97px)"
        ]
        [ span
            [ css "float" "right"
            , css "margin" "4px"
            , css "color" "grey"
            ]
            [ text (("id=" ++ (toString document.id)) ++ (" | date=" ++ (toString document.time_stamp)))
            , br [][]
            ]
        , h1 [] [ text document.title]
        , div
            [ css "overflow-y" "auto"
            , css "height" "inherit"
            ]
            [ text document.fulltext]
        ]
