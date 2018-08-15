module Termsview exposing (view)

import Model exposing (..)
import Msg exposing (..)
import Term exposing (..)
import Topic exposing (defaultTopic, Topic)
import Request
import IconSet exposing (..)

import Array
import Dict
import ContainerCache
import Html exposing (Html, text)
import Html.Events
import Material
import Material.Options exposing (Property, css, cs, div, span, center, onClick, onMouseEnter, onMouseLeave, nop)
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.List as Lists

view : Model -> Property c Msg -> Int -> String -> Topic -> Bool -> Html Msg
view model flex slotId slotName parent withHead =
    div
        [ cs "slot"
        , if model.settings.frame == Mobile
        then css "width" "100%"
        else flex
        , Elevation.e0
        , primaryColor
        , css "display" "inline-flex"
        ]
        [ if (withHead)
        then div
                [ css "height" "45px"
                , if model.settings.frame == Mobile
                then css "width" "100%"
                else css "max-width" "400px"
                , center
                ]
                [ iconTerm model.mdl [ css "margin" "5px"]
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
        , Lists.ul
            [ cs "slot__content"
            , if model.settings.frame == Mobile
            then css "width" "100%"
            else css "max-width" "400px"
            , css "padding" "8px 0 0 0"
            , Material.Options.id ("slot" ++ (toString slotId))
            ]
            (moveActions
                ManageTermsCache
                model.termsCache
                (Maybe.withDefault 0 (Dict.get slotName model.termsDict))
                slotId
                (\x y ->
                    Lists.li []
                        [ Lists.content x y])
                [ cs "mdl-button"
                , cs "mdl-button--raised"
                , css "overflow" "visible"
                , cs "item"
                ]
                (List.indexedMap (terms2ListItem model parent slotId) model.terms)
            )
        ]

terms2ListItem : Model -> Topic -> Int -> Int -> Term -> Html Msg
terms2ListItem model parent slotId id term =
    Lists.li
        [ css "overflow" "visible"
        ]
        [ Lists.content
            [ cs "mdl-button"
            , cs "mdl-button--raised"
            , css "overflow" "visible"
            , cs "item"
            , center
            ]
            [ span
                [ css "width" "calc(100% - 48px)"
                , onClick
                    (Request.createNewDocsContainer False model parent (Just term) (slotId + 1))
                    -- (ExecCmd (slotId + 1) "300px" (Request.loadBestDocs NewDocs defaultTopic (Just term) "RELEVANCE" (slotId + 1)))
                ]
                [ if (model.settings.showRelevance)
                  then text (term.name ++ " (" ++ (toString (Maybe.withDefault 0 term.relevance)) ++ ")")
                  else text term.name
                ]
            , span
                [ onClick
                    (ExecCmd (slotId + 1) "300px" (Request.loadAutocompleteTerms term.name (slotId + 1)))
                ]
                [ iconTopic model.mdl (iconHighlighted model.settings (slotId, id))]
            , span
                [ onClick
                    (Request.createNewDocsContainer False model parent (Just term) (slotId + 1))
                --     (ExecCmd (slotId + 1) "300px" (Request.loadBestDocs NewDocs defaultTopic (Just term) "RELEVANCE" (slotId + 1)))
                ]
                [ iconDoc model.mdl (iconHighlighted model.settings (slotId, id))]
            ]
        ]
