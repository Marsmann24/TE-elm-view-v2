module TermsDocumentsview exposing (view)

import Model exposing (..)
import Msg exposing (Msg(..))
import Topic
import Termsview
import Documentsview

import IconSet exposing (..)

import Html exposing (Html, text)
import Material.Options exposing (Property, div, span, center, css, cs, onClick)
import Material.Button as Button
import Material.Icon as Icon
import Material.Elevation as Elevation

view : Model -> Property c Msg-> Int -> String -> Html Msg
view model flex slotId name =
    div [ cs "slot"
        , if model.settings.frame == Mobile
        then css "width" "100%"
        else flex
        , Elevation.e0
        , primaryColor
        , css "display" "inline-flex"
        ]
        [ div
            [ css "height" "45px"
            , center
            ]
            [ iconTopicDetail model.mdl [ css "margin" "5px"]
            , span
                [ css "width" "calc(100% - 64px)"
                , css "text-align" "left"
                ]
                [ text name]
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
        , div [ cs "flex__row"]
            [ Termsview.view model (css "flex" "1 1 50%") slotId "" Topic.defaultTopic False
            , Documentsview.view model (css "flex" "1 1 50%") slotId "" False
            ]
        ]
