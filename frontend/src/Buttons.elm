module Buttons exposing (..)

import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

-- ####
-- ####    MODEL
-- ####

type alias Model =
    {
        newEnabled : Bool
        , restartEnabled : Bool
        , undoEnabled : Bool
    }


init : Model
init =
    {
        newEnabled = False
        , restartEnabled = False
        , undoEnabled =False
    }

-- ####
-- ####    VIEW
-- ####


view : Model -> Html Msg
view { newEnabled, restartEnabled, undoEnabled } =
    div [ class "buttons-container" ]
        [
        if newEnabled then
            button [ class "button", onClick NewClicked ] [ text "New game" ]
        else
            button [ class "button button-disabled" ] [ text "New game" ]

        , if restartEnabled then
            button [ class "button", onClick RestartClicked ] [ text "Restart" ]
        else
            button [ class "button button-disabled" ] [ text "Restart" ]

        , if undoEnabled then
            button [ class "button", onClick UndoClicked ] [ text "Undo" ]
        else
            button [ class "button button-disabled" ] [ text "Undo" ]
        ]


-- ####
-- ####    UPDATE
-- ####


type Msg =
    NewClicked
    | RestartClicked
    | UndoClicked



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


