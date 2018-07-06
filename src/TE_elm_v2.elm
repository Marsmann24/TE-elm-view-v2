module TE_elm_v2 exposing (..)

-- import Mainview_v1
-- import Mainview_v2
-- import Mainview_v3
import Mainview_v4
import Update
import Init

import Model exposing (Model)
import Msg exposing (Msg(..))

import Html
import Material.Layout as Layout

main : Program Never Model Msg
main =
    Html.program
        { init = Init.init
        , update = Update.update
        , view = view
        , subscriptions = subscriptions
        }

view model =
    Mainview_v4.view model
    -- ButtonTest.view model

subscriptions : Model -> Sub Msg
subscriptions model =
    -- Sub.none
    Layout.subs Mdl model.mdl
