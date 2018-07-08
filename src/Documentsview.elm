module Documentsview exposing (view)

import Model exposing (..)
import Msg exposing (..)
import Document exposing (..)
import Request
import IconSet exposing (..)

import Html exposing (Html, text)
import Material
import Material.Options exposing (Property, css, cs, div, span, center, onClick, onMouseEnter, onMouseLeave, onMouseDown, onMouseUp)
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.Color as Color
import Material.Card as Card
import Dict

view : Model -> Property c Msg -> Int -> String -> Bool -> Html Msg
view model flex slotId slotName withHead =
    div
        [ cs "slot"
        , flex
        , Elevation.e0
        , primaryColor
        , css "display" "inline-flex"
        ]
        [ if (withHead)
        then div
            [ css "height" "45px"
            , css "max-width" "400px"
            , center
            ]
            [ iconDoc model.mdl [ css "margin" "5px"]
            , span
                [ css "width" "calc(100% - 64px)"
                , css "text-align" "left"
                ]
                [ text slotName]
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
        else div [] []
        , div
            [ cs "slot__content"
            , css "max-width" "400px"
            , Material.Options.id ("slot" ++ (toString slotId))
            ]
            (moveActions
                ManageDocsCache
                model.docsCache
                (Maybe.withDefault 0 (Dict.get slotName model.docsDict))
                slotId
                (\x y ->
                    Card.view
                        [ Color.background Color.primary
                        , css "width" "94%"
                        , css "margin" "4% 8% 4% 3%"
                        ]
                        [ Card.title x y])
                []
                (List.map2 (doc2CardView slotId) model.docs (List.range 1 (List.length model.docs)))
            )
        ]

doc2CardView : Int -> Doc -> Int -> Html Msg
doc2CardView slotId doc cardID =
    Card.view
        [ css "height" "120px"
        , css "width" "94%"
        , css "margin" "4% 8% 4% 3%"
        , Color.background Color.primary
        , Elevation.e0
        , onClick (ExecCmd (slotId + 1) "1000px" (Request.loadDoc (slotId + 1) doc))
        ]
        [ Card.title
            [ css "padding" "4px"
            ]
            [ Card.head
                [ Color.text Color.white
                , css "font-size" "14px"
                , css "width" "100%"
                ]
                [ span
                    [ css "width" "calc(100% - 48px)"]
                    [ text doc.title]
                ]
            , span
                [ Color.text (Color.color Color.Grey Color.S200)
                , css "padding" "2px"
                , css "font-size" "8px"
                , css "align-self" "right"
                ]
                [ text (("id=" ++ (toString doc.id)) ++ (" | date=" ++ (toString doc.time_stamp))) ]
            ]
        , Card.text
            [ Color.text (Color.white)
            , css "padding" "4px"
            , css "font-size" "10px"
            ]
            [ text doc.snippet ]
        ]
