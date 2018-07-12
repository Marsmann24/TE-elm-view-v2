module Startpage exposing (view)

import Msg exposing (Msg(..))
import Model exposing (Settings, Frame(..))
import IconSet exposing (primaryColor)

import Html exposing (Html, text)
import Material.Options exposing (div, cs, css, center, disabled, onClick)
import Material.Card as Card
import Material.Color as Color

view : Settings -> Html Msg
view settings =
    div [ cs "flex__row"
        , css "height" "100%"
        ]
        [ div
            [ center
            , cs "slot"
            , css "heigth" "100%"
            , css "margin-top" "-50px"
            ]
            [ Card.view
                  [ css "width" "400px"
                  , cs "startpage-card"
                  , onClick (Open ViewTopics)
                  ]
                  [ Card.text
                      [ css "height" "225px"
                      ]
                      []
                  , Card.title [ ]
                      [ Card.head [ ] [ text "View Topics" ]
                      , Card.subhead [ ] [ text "Browse topics to get an overview over the data." ]
                      ]
                  ]
              ]
        , div
            [ center
            , cs "slot"
            , css "heigth" "100%"
            , css "margin-top" "-50px"
            ]
            [ Card.view
                  [ css "width" "400px"
                  , cs "startpage-card-disabled"
                  ]
                  [ Card.text
                      [ css "height" "225px"
                      ]
                      [ text "Coming Soon"]
                  , Card.title [ ]
                      [ Card.head [ ] [ text "Topic Statistics" ]
                      , Card.subhead [ ] [ text "Timelines and other Statistics about a Topic or all Topics." ]
                      ]
                  ]
              ]
        , div
            [ center
            , cs "slot"
            , css "heigth" "100%"
            , css "margin-top" "-50px"
            ]
            [ Card.view
                  [ css "width" "400px"
                  , cs "startpage-card-disabled"
                  ]
                  [ Card.text
                      [ css "height" "225px"
                      ]
                      [ text "Coming Soon"]
                  , Card.title [ ]
                      [ Card.head [ ] [ text "Compare Topics" ]
                      , Card.subhead [ ] [ text "Compare two or more Topics and join Topics together." ]
                      ]
                  ]
             ]
        ]
