module Topicsview exposing (view)

import Model exposing (..)
import Msg exposing (..)
import Topic exposing (..)
import Request
import IconSet exposing (..)

import Html exposing (Html, text)
import Html.Attributes exposing (style, class)
import Material
import Material.Options exposing (Property, css, cs, div, span, onClick, onMouseEnter, onMouseLeave, center, disabled)
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.Chip as Chip

view : Model -> Property c Msg -> Int -> String -> Html Msg
view model flex slotId slotName =
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
            , if model.settings.frame == Mobile
            then css "width" "100%"
            else css "max-width" "400px"
            , center
            ]
            [ iconTopic model.mdl [ css "margin" "5px"]
            , span
                [ css "width" "calc(100% - 64px)"
                , css "text-align" "left"
                ]
                [ text slotName
                ]
            , Button.render Mdl [slotId] model.mdl
                [ cs "slot__close_button"
                , Button.fab
                , Button.minifab
                , Button.raised
                , onClick (DeleteSlot slotId)
                , if (slotId == 0)
                    then disabled True
                    else disabled False
                ]
                [ Icon.i "close" ]
            ]
        , div
            [ cs "slot__content"
            , if model.settings.frame == Mobile
            then css "width" "100%"
            else css "max-width" "400px"
            , css "padding" "8px 0 0 0"
            ]
            (List.indexedMap (topic2Chip model model.mdl model.settings slotId) model.topics)
        ]

topic2Chip : Model -> Material.Model -> Settings -> Int -> Int -> Topic -> Html Msg
topic2Chip model mdl settings slotId id topic =
    Chip.span
        [ css "width" "calc(100% - 40px)"
        , css "margin" "6px 4px"
        , center
        , cs "item"
        ]
        [ Chip.content
            [ css "width" "100%"
            , center
            ]
            [ span
                [ css "width" "calc(100% - 58px)"
                , css "overflow" "hidden"
                , css "margin-right" "10px"
                , onClick
                    (Batch
                        [ (Request.createNewDocsContainer True model topic Nothing (slotId + 1))
                        , (Request.createNewTermsContainer True model topic (slotId + 1))
                        , (ExecCmd (slotId + 1) "600px" Cmd.none)
                        ])
                ]
                [ span [ css "margin-right" "10px"] [ text (("Topic " ++ (toString topic.id)) ++ ": ")]
                , text
                    (String.concat
                        (List.intersperse
                            ", "
                            (List.take 2
                                (List.map .name topic.top_terms)
                            )
                        )
                    )
                , text " ... "
                ]
            , span
                [ onClick
                    (Request.createNewTermsContainer False model topic (slotId + 1))
                , center
                ]
                [ iconTerm mdl (iconHighlighted settings (slotId, id))]
            , span
                [ onClick
                    (Request.createNewDocsContainer False model topic Nothing (slotId + 1))
                -- , onClick
                --     (ExecCmd (slotId + 1) "300px" (Request.loadBestDocs NewDocs topic Nothing "RELEVANCE" (slotId + 1)))
                , center
                ]
                [ iconDoc mdl (iconHighlighted settings (slotId, id))]
            ]
        ]
