module Mainview_v4 exposing (view)

import Model exposing (..)
import Msg exposing (..)
import Startpage
import Showdocumentview
import Topicsview
import TermsDocumentsview
import Documentsview
import Termsview
import Searchview
import IconSet exposing (..)

import Array
import Html exposing (Html, text, h3, p)
import Html.Events exposing (keyCode)
import Material.Options exposing (Property, css, cs, center, div, span, onToggle, onClick, onInput, on, dispatch)
import Material.Icon as Icon
import Material.Color as Color
import Material.Scheme as Scheme
import Material.Layout as Layout
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Button as Button
import Material.Card as Card
import Material.Elevation as Elevation
import Json.Decode as Json

view : Model -> Html Msg
view model =
        let settings = model.settings
        in
        Layout.render Mdl model.mdl
            [ Layout.fixedHeader]
            { header =
                (if settings.error == ""
                then
                    [ span []
                        [ viewSearch model
                        , iconHome model.mdl [ onClick (Open Startpage)]
                        ]
                    ]
                else
                    [ viewSearch model
                    , iconHome model.mdl [ onClick (Open Startpage)]
                    , span
                        []
                        [ text settings.error
                        ]
                    ]
                )
            , drawer = [ viewSwitch model]
            , tabs = ( [], [])
            , main =
                [ (if settings.search
                    then
                        span
                            [ cs "search_overlay"
                            , onClick ResetSettings
                            ]
                            [ Searchview.view model (css "flex" "1 1 100%")]
                    else
                        span [] []
                    )
                , viewBody model
                ]
            }
        |> Scheme.topWithScheme Color.Green Color.Orange

viewSearch : Model -> Html Msg
viewSearch model =
    span
        [ cs "search_box"
        , primaryColor
        ]
        [ Textfield.render Mdl [7] model.mdl
            [ Textfield.label "Search"
            , Textfield.floatingLabel
            , Textfield.text_
        --    , Textfield.value <value> to set the value manualy if the label is not floating, wehn it should
        --    , Textfield.expandable "id-of-expandable-1"
        --    , Textfield.expandableIcon "search"
            , onInput Search
            , onEnterPressed (AdvancedSearch model.settings.search4)
            , dispatch Batch
            ]
            [ ]
        ]

onEnterPressed : Msg -> Property c Msg
onEnterPressed msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
    on "keydown" <| Json.andThen isEnter keyCode

viewSwitch : Model -> Html Msg
viewSwitch model =
    let oldSettings = model.settings
    in
    div
        []
        [ h3 [] [ text "Einstellungen"]
        -- , Toggles.switch Mdl [1] model.mdl
        --     [ css "margin" "5px"
        --     , onToggle (Toggle { oldSettings | view2 = not model.settings.view2})
        --     , Toggles.value model.settings.view2
        --     ]
        --     [ text "main view 2" ]
        -- , Toggles.switch Mdl [2] model.mdl
        --     [ css "margin" "5px"
        --     , onToggle (Toggle { oldSettings | showRelevance = not model.settings.showRelevance})
        --     , Toggles.value model.settings.showRelevance
        --     ]
        --     [ text "show term relevance"]
        -- , Toggles.switch Mdl [3] model.mdl
        --     [ css "margin" "5px"
        --     , onToggle (Mobile (not model.settings.mobile))
        --     , Toggles.value model.settings.mobile
        --     ]
        --     [ text "mobile version"]
        ]

viewBody : Model -> Html Msg
viewBody model =
    case model.settings.frame of
        Startpage ->
            Startpage.view model.settings
        _ ->
        div [ Elevation.e4
            , cs "flex__row"
            , css "height" "100%"
            ]
            [ div
                [ css "white-space" "nowrap"
                , css "display" "inline-block"
                , css "overflow-y" "hidden"
                , css "overflow-x" "auto"
                , css "height" "100%"
                , Material.Options.id "board"
                ]
                (List.indexedMap (slot model) model.slots)
            ]

slot : Model -> Int -> View -> Html Msg
slot model slotId view =
    case view of
        TopicsView name topics ->
            Topicsview.view { model | topics = topics} (css "width" "300px") slotId name
        TermsDocumentsView name terms docs ->
            TermsDocumentsview.view { model | terms = terms, docs = docs} (css "width" "600px") slotId name
        TermsView name terms ->
            Termsview.view { model | terms = terms} (css "width" "300px") slotId name True
        DocumentsView name docs ->
            Documentsview.view { model | docs = docs} (css "width" "300px") slotId name True
        ShowdocumentView document ->
            Showdocumentview.view model document (css "width" "1000px") slotId
        Empty width ->
            div [ cs "slot"
                , css "width" width
                , Elevation.e0
                , primaryColor
                , css "display" "inline-flex"
                , css "height" "calc(100% - 5px)"
                ]
                [ Icon.view "autorenew" [ css "margin" "5px"]
                , p [] [ text "loading"]
                ]
        _ ->
            div [ css "width" "100px"]
                [ text "Error"]
