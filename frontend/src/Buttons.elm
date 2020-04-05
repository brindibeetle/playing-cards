module Buttons exposing (..)

import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

-- ####
-- ####    VIEW
-- ####


view : Helper msg -> Html msg
view {  undoClicked, newClicked, restartClicked, newEnabled, restartEnabled, undoEnabled } =
    div [ class "buttons-container" ]
        [
        if newEnabled then
            button [ class "button", onClick newClicked ] [ text "New game" ]
        else
            button [ class "button button-disabled" ] [ text "New game" ]

        , if restartEnabled then
            button [ class "button", onClick restartClicked ] [ text "Restart" ]
        else
            button [ class "button button-disabled" ] [ text "Restart" ]

        , if undoEnabled then
            button [ class "button", onClick undoClicked ] [ text "Undo" ]
        else
            button [ class "button button-disabled" ] [ text "Undo" ]
        ]


-- ####
-- ####    HELPER
-- ####


type alias Helper msg =
    { undoClicked : msg
    , newClicked : msg
    , restartClicked : msg

    , newEnabled : Bool
    , restartEnabled : Bool
    , undoEnabled : Bool
    }


