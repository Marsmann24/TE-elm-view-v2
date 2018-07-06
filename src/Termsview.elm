module Termsview exposing (view)

import Model exposing (..)
import Msg exposing (..)
import Term exposing (..)
import Topic exposing (defaultTopic)
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

view : Model -> Property c Msg -> Int -> String -> Bool -> Html Msg
view model flex slotId slotName withHead =
    let containerId = (Maybe.withDefault 0 (Dict.get slotName model.termsDict))
        thisCM = model.termsCache
        thisCache = Maybe.withDefault ContainerCache.defaultContainer (Array.get containerId thisCM.arrayOfContainer)
        thisMeta = thisCache.meta
        thisCurrPage = thisMeta.currPage
        nextMeta = { thisMeta | currPage = thisCurrPage + 1}
        nextCache = { thisCache | meta = nextMeta}
        nextCM = { thisCM | arrayOfContainer = Array.fromList [nextCache]}
    in
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
            , css "max-width" "400px"
            ]
            (List.concat
                [ if ((Maybe.withDefault ContainerCache.defaultContainer (Array.get containerId model.termsCache.arrayOfContainer)).meta.currPage == 0)
                    then []
                    else
                        [ Lists.li []
                            [ Lists.content
                                [ cs "mdl-button"
                                , cs "mdl-button--raised"
                                , css "overflow" "visible"
                                , cs "item"
                                , center
                                , onClick (ManageTermsCache (ContainerCache.PageUpdate containerId (ContainerCache.PrevPage)))
                                ]
                                [ Icon.i "expand_less"]
                            ]
                        ]
                , (List.indexedMap (terms2ListItem model.mdl model.settings slotId) model.terms)
                , (case (ContainerCache.getCurrPageDataFromContainer nextCM containerId) of
                    Just [] ->
                        []
                    Just a ->
                        [ Lists.li []
                            [ Lists.content
                                [ cs "mdl-button"
                                , cs "mdl-button--raised"
                                , css "overflow" "visible"
                                , cs "item"
                                , center
                                , onClick (ManageTermsCache (ContainerCache.PageUpdate containerId (ContainerCache.NextPage)))
                                ]
                                [ Icon.i "expand_more"]
                            ]
                        ]
                    _ ->
                        [])
                ])
        ]

terms2ListItem : Material.Model -> Settings -> Int -> Int -> Term -> Html Msg
terms2ListItem mdl settings slotId id term =
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
                    (ExecCmd (slotId + 1) "300px" (Request.loadBestDocs NewDocs defaultTopic (Just term) "RELEVANCE" (slotId + 1)))
                ]
                [ if (settings.showRelevance)
                  then text (term.name ++ " (" ++ (toString (Maybe.withDefault 0 term.relevance)) ++ ")")
                  else text term.name
                ]
            , span
                [ onClick
                    (ExecCmd (slotId + 1) "300px" (Request.loadAutocompleteTerms term.name (slotId + 1)))
                ]
                [ iconTopic mdl (iconHighlighted settings (slotId, id))]
            , span
                [ onClick
                    (ExecCmd (slotId + 1) "300px" (Request.loadBestDocs NewDocs defaultTopic (Just term) "RELEVANCE" (slotId + 1)))
                ]
                [ iconDoc mdl (iconHighlighted settings (slotId, id))]
            ]
        ]
