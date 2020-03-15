module Home exposing (..)

import Array exposing (Array)
import Card exposing (Card, cardPlaceholder)
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)
import Html5.DragDrop as DragDrop


type alias Model =
    {
        homes : Array ( Maybe Card )
        , dragDrop : DragDrop.Model ( Int, Int ) Int
    }


init : Model
init =
    {
        homes = Array.repeat numberOfHomes Nothing
        , dragDrop = DragDrop.init
    }


numberOfHomes : Int
numberOfHomes = 4


-- ####
-- ####    DRAGHELPER
-- ####


type alias DragHelper msg =
    { maybeDraggedCard : Maybe Card, draggedNumberOfCards : Int, droppableAttribute : Int -> List (Attribute msg) }


-- ####
-- ####    VIEW
-- ####


view : Model -> DragHelper msg -> Html msg
view model dragHelper =
    div [ class "homes-container" ]
        ( Array.indexedMap (viewHome dragHelper) model.homes |> Array.toList )


viewHome : DragHelper msg -> Int -> Maybe Card -> Html msg
viewHome dragHelper index maybeCard =
    let
        { maybeDraggedCard, draggedNumberOfCards, droppableAttribute } = dragHelper

        cardsSuccessive =
            case maybeDraggedCard of
                Nothing ->
                    False

                Just draggedCard ->
                    Card.cardsSuccessiveHome maybeCard draggedCard
    in
    case ( maybeCard, cardsSuccessive )  of
        ( Nothing, False ) ->
            div []
                [ cardPlaceholder
                ]

        ( Nothing, True ) ->
            div ( droppableAttribute index )
                [ cardPlaceholder
                ]

        ( Just card, False ) ->
            div []
                [ cardPlaceholder
                , div
                    [ class "card-home" ]
                    [ Card.view card ]
                ]

        ( Just card, True ) ->
            div ( droppableAttribute index )
                [ cardPlaceholder
                , div
                    [ class "card-home" ]
                    [ Card.view card ]
                ]


-- ####
-- ####    UPDATE
-- ####


-- ####
-- ####    HELPER
-- ####


pushCard : Int -> Card -> Model -> Model
pushCard homeId card model =
    { model
    | homes = Array.set homeId ( Just card ) model.homes
    }
