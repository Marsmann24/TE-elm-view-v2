module Mainview_v4 exposing (view)

import Model exposing (..)
import Term
import Topic
import Msg exposing (..)
import Startpage
import Showdocumentview
import Topicsview
import TermsDocumentsview
import Documentsview
import Termsview
import Searchview
import IconSet exposing (..)

import ContainerCache
import Array
import Dict
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
                    [ span []
                        [ viewSearch model
                        , iconHome model.mdl [ onClick (Open Startpage)]
                        ]
                    , span
                        []
                        [ text settings.error
                        ]
                    ]
                )
            , drawer = [ viewSwitch model]
            , tabs = ( [], [])
            , main =
                -- [ ButtonTest.view model
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
        Mobile ->
            let listLengthSub1 = ((List.length model.slots) - 1)
            in
            div [ Elevation.e4
                , css "height" "100%"
                ]
                [ div []
                    -- [ css "height" "40px"]
                    (List.indexedMap toHistoryButton model.slots)
                , div
                    [ css "height" "calc(100% - 36px)"
                    , cs "flex__row"]
                    [ (slot model listLengthSub1 (Maybe.withDefault (Empty "100%") (List.head (List.reverse model.slots))))]
                ]
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
            Termsview.view { model | terms = terms} (css "width" "300px") slotId name Topic.defaultTopic True
        DocumentsView name docs ->
            Documentsview.view { model | docs = docs} (css "width" "300px") slotId name True
        ShowdocumentView document ->
            Showdocumentview.view model document (css "width" "1000px") slotId
        TermsContainerSlot name parent ->
            let inContainer =
                    (ContainerCache.getCurrPageDataFromContainer model.termsCache (Maybe.withDefault 0 (Dict.get name model.termsDict)))
            in
            case inContainer of
                Just terms ->
                    Termsview.view { model | terms = terms} (css "width" "300px") slotId name parent True
                _ ->
                    loadingView "300px"
        DocsContainerSlot name ->
            let inContainer =
                    (ContainerCache.getCurrPageDataFromContainer model.docsCache (Maybe.withDefault 0 (Dict.get name model.docsDict)))
            in
            case inContainer of
                Just docs ->
                    Documentsview.view { model | docs = docs} (css "width" "300px") slotId name True
                _ ->
                    loadingView "300px"
        CombinedView name view1 view2 ->
            div
                [ cs "slot"
                , Elevation.e0
                , primaryColor
                , css "display" "inline-flex"
                ]
                [ div
                    [ css "white-space" "nowrap"
                    , css "display" "inline-block"
                    , css "overflow-y" "hidden"
                    , css "height" "100%"
                    ]
                    [ slot model slotId view1
                    , slot model slotId view2
                    ]
                ]
        Empty width ->
            loadingView width
        _ ->
            div [ css "width" "100px"]
                [ text "Error"]

toHistoryButton : Int -> View -> Html Msg
toHistoryButton slotId view =
    case view of
        TopicsView name _ ->
            arrawRight name slotId
        TermsDocumentsView name _ _ ->
            arrawRight name slotId
        TermsView name _ ->
            arrawRight name slotId
        DocumentsView name _ ->
            arrawRight name slotId
        ShowdocumentView document ->
            arrawRight document.title slotId
        TermsContainerSlot name _ ->
            arrawRight name slotId
        DocsContainerSlot name ->
            arrawRight name slotId
        CombinedView name view1 view2 ->
            arrawRight name slotId
        _ ->
            arrawRight " ... " slotId


arrawRight : String -> Int -> Html Msg
arrawRight name slotId =
    span
        [ css "margin-left" "1px"
        , css "padding" "14px 4px 2px 2px"
        , css "border" "2px solid #78909c"
        , css "border-radius" "3px 6px 6px 3px"
        , Color.background (Color.color Color.BlueGrey Color.S200)
        , onClick (DeleteSlot (slotId + 1))
        ]
        [ span
            [ cs "arrawRight"
            ]
            []
        , span
            [ css "font-size" "18px"
            ]
            [ text name]
        ]
