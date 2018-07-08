module IconSet exposing (..)

import Msg exposing (..)
import Model exposing (Settings, Model)
import Update exposing (scroll2Top, scroll2Bottom)

import ContainerCache
import Html exposing (Html, text)
import Material
import Material.Options exposing (Property, div, span, cs, css, onClick, center)
import Material.Icon as Icon exposing (size48)
import Material.Tooltip as Tooltip
import Material.Spinner as Spinner
import Material.Elevation as Elevation
import Array

iconHome : Material.Model -> List (Icon.Property Msg) -> Html Msg
iconHome mdl args =
    span [cs "homeIcon"]
        [ Icon.view "home" ((size48 :: (Tooltip.attach Mdl [0]) :: args))
        , Tooltip.render Mdl [0] mdl
            [ css "position" "relative"]
            [ text "home"]
        ]

iconTopic : Material.Model -> List (Icon.Property Msg) -> Html Msg
iconTopic mdl args =
    span []
        [ Icon.view "subject" ((Tooltip.attach Mdl [1]) :: args)
        , Tooltip.render Mdl [1] mdl
            [ css "position" "relative"]
            [ text "topic"]
        ]

iconTopicDetail : Material.Model -> List (Icon.Property Msg) -> Html Msg
iconTopicDetail mdl args =
    span []
        [ Icon.view "tablet" ((Tooltip.attach Mdl [2]) :: args)
        , Tooltip.render Mdl [2] mdl
            [ css "position" "relative"]
            [ text "topic details"]
        ]

iconTerm : Material.Model -> List (Icon.Property Msg) -> Html Msg
iconTerm mdl args =
    span []
        [ Icon.view "list" ((Tooltip.attach Mdl [3]) :: args)
        , Tooltip.render Mdl [3] mdl
            [ css "position" "relative"]
            [ text "term"]
        ]

iconDoc :  Material.Model ->List (Icon.Property Msg) -> Html Msg
iconDoc mdl args =
    span []
        [ Icon.view "description" ((Tooltip.attach Mdl [4]) :: args)
        , Tooltip.render Mdl [4] mdl
            []
            [ text "doc"]
        ]

iconDocument : Material.Model -> List (Icon.Property Msg) -> Html Msg
iconDocument mdl args =
    span []
        -- [ Icon.view "chrome_reader_mode" ((Tooltip.attach Mdl [4]) :: args)
        [ Icon.view "description" ((Tooltip.attach Mdl [4]) :: args)
        , Tooltip.render Mdl [4] mdl
            []
            [ text "document"]
        ]

moveActions : (ContainerCache.ContainerModelMsg (List a) -> Msg) -> ContainerCache.ContainerModel (List a) -> Int -> Int -> (List (Property c Msg) -> List (Html Msg) -> Html Msg) -> List (Property c Msg) -> List (Html Msg) -> List (Html Msg)
moveActions cacheMsg cache containerId slotId actionHtml actionStyle innerHtml =
    let thisCM = cache
        thisCache = Maybe.withDefault ContainerCache.defaultContainer (Array.get containerId thisCM.arrayOfContainer)
        thisMeta = thisCache.meta
        thisCurrPage = thisMeta.currPage
        nextMeta = { thisMeta | currPage = thisCurrPage + 1}
        nextCache = { thisCache | meta = nextMeta}
        nextCM = { thisCM | arrayOfContainer = Array.fromList [nextCache]}
    in
    (List.concat
        [ if ((Maybe.withDefault ContainerCache.defaultContainer (Array.get containerId cache.arrayOfContainer)).meta.currPage == 0)
            then []
            else
                [ actionHtml
                    (List.append
                        actionStyle
                        [ center
                        , css "cursor" "pointer"
                        , onClick
                            (Batch
                                [ (cacheMsg (ContainerCache.PageUpdate containerId (ContainerCache.PrevPage)))
                                , ExecCmd -1 "" (scroll2Bottom ("slot" ++ (toString slotId)))
                                ]
                            )
                        ])
                    [ Icon.i "expand_less"]
                ]
        , innerHtml
        , (case (ContainerCache.getCurrPageDataFromContainer nextCM containerId) of
            Just [] ->
                []
            _ ->
                [ actionHtml
                    (List.append
                        actionStyle
                        [ center
                        , css "cursor" "pointer"
                        , onClick
                            (Batch
                                [ (cacheMsg (ContainerCache.PageUpdate containerId (ContainerCache.NextPage)))
                                , ExecCmd -1 "" (scroll2Top ("slot" ++ (toString slotId)))
                                ]
                            )
                        ])
                    [ Icon.i "expand_more"]
                ])
        ])

loadingView : String -> Html Msg
loadingView width =
    div [ cs "slot"
        , css "width" width
        , Elevation.e0
        , primaryColor
        , css "display" "inline-flex"
        , css "height" "calc(100% - 5px)"
        ]
        [ span
            [ css "margin" "12px"
            ]
            [ text "loading..."]
        , span [ cs "loading"]
            [ Spinner.spinner
                [ Spinner.active True
                , Spinner.singleColor True
                ]
            -- , Icon.view "autorenew"
            --     [ css "margin" "5px"
            --     , Icon.size48
            --     ]
            ]
        ]

iconHighlighted : Settings -> (Int, Int) -> List (Icon.Property Msg)
iconHighlighted settings id =
-- if (id == settings.selectedItem)
-- then
--     [ Icon.size48
--     , Color.text Color.primary
--     , Color.background Color.primaryContrast
--     , css "border-radius" "10px"
--     , css "margin" "10px"
--     , css "padding" "10px"
--     ]
-- else
    []

primaryColor : Property c Msg
--primaryColor = Color.background Color.primaryContrast
primaryColor = cs "primaryColor"

generalBackgroundColor : Property c Msg
generalBackgroundColor = cs "generalBackgroundColor"
