module IconSet exposing (..)

import Msg exposing (..)
import Model exposing (Settings)

import Html exposing (Html, text)
import Material
import Material.Options exposing (Property, span, cs, css)
import Material.Icon as Icon exposing (size48)
import Material.Tooltip as Tooltip

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
