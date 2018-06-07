module Searchview exposing (view)

import Model exposing (..)
import Msg exposing (..)
import Request
import Topic exposing (Topic)
import Term exposing (Term)
import IconSet exposing (..)

import Html exposing (Html, text)
import Material.Options exposing (Property, div, span, cs, css, center, onClick)
import Material.List as Lists
import Material.Elevation as Elevation

view : Model -> Property c Msg -> Html Msg
view model flex =
    div
        [ cs "search_results"
        --, css "flex" flex
        , primaryColor
        --, center
        ]
        [ Lists.ul
            [ css "overflow" "auto"]
            --(List.append
            --    (advancedSearch model.settings.search4)
                (searchresult2ListItems model.settings.searchResult)
            --)
        ]

advancedSearch : String -> List (Html Msg)
advancedSearch search4 =
    [ if (String.startsWith "topic:" search4)
      then
          let search4topic = (String.dropLeft 6 search4)
          in
        (li "bubble_chart"
            ("Search for Topics with " ++ search4topic)
            (ExecCmd 1 "300px" (Request.loadSearchTopics search4topic))
        )
      else span [] []
    , if (String.startsWith "term:" search4)
      then
          let search4term = (String.dropLeft 5 search4)
          in
          (li "list"
            ("Search for Terms with " ++ search4term)
            (ExecCmd 1 "300px" (Request.loadSearchTerms search4term))
        )
      else span [] []
    , if (String.startsWith "document:" search4)
      then
          let search4doc = (String.dropLeft 9 search4)
          in
          (li "art_track"
            ("Search for Documents with " ++ search4doc)
            (ExecCmd 1 "300px" (Request.loadSearchDocs search4doc False "RELEVANCE"))
        )
      else span [] []
    ]

searchresult2ListItems : SearchResult -> List (Html Msg)
searchresult2ListItems result =
    case result of
        TermResult list ->
            let lia : Term -> Html Msg
                lia a =
                    li "list" a.name (Found (TermsView "Terms" [a]))
            in
            List.map lia list
        TopicResult list ->
            let lia : Topic -> Html Msg
                lia a =
                    li "bubble_chart" (toString a.id) (Found (TopicsView "Topics" [a]))
            in
            List.map lia list
        DocumentResult a ->
            [ li "art_track" a.title (Found (DocumentsView "Documents" [a]))]

li : String -> String -> Msg -> Html Msg
li icon label msg =
    Lists.li
        [ Elevation.e4
        , css "margin" "20px 0 20px 0"
        ]
        [ Lists.content
            [ onClick msg]
            [ Lists.icon icon []
            , text label
            ]
        ]
