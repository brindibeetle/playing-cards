module Space exposing (..)

import Array exposing (Array)
import Card exposing (Card, cardPlaceholder)
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)
import Html5.DragDrop as DragDrop


type alias Model =
    {
        spaces : Array ( Maybe Card )
        , dragDrop : DragDrop.Model ( Int, Int ) Int
    }

init : Model
init =
    {
        spaces = Array.repeat numberOfSpaces Nothing
        , dragDrop = DragDrop.init
    }

numberOfSpaces : Int
numberOfSpaces = 4


-- ####
-- ####    DRAGHELPER
-- ####


type alias DragHelper msg =
    { maybeDragFromSpaceId : Maybe Int, draggedNumberOfCards : Int, droppableAttribute : Int -> List (Attribute msg), draggableAttribute : Int -> List (Attribute msg) }


-- ####
-- ####    VIEW
-- ####

view : Model -> DragHelper msg -> Html msg
view model dragHelper =
    div [ class "spaces-container" ]
        ( Array.indexedMap (viewSpace dragHelper) model.spaces |> Array.toList )


viewSpace : DragHelper msg -> Int -> Maybe Card -> Html msg
viewSpace dragHelper index maybeCard =
    let
        { maybeDragFromSpaceId, draggedNumberOfCards, droppableAttribute, draggableAttribute } = dragHelper
    in
    case maybeCard  of
        Nothing ->
            if draggedNumberOfCards == 1 then
                div ( droppableAttribute index )
                    [ cardPlaceholder
                    ]
            else
                div []
                    [ cardPlaceholder
                    ]

        Just card ->
            if ( maybeDragFromSpaceId |> Maybe.withDefault 99 ) == index then
                div ( List.append ( draggableAttribute index ) ( droppableAttribute index ) )
                    [ cardPlaceholder
                    , div
                        [ class "card-space" ]
                        [ Card.view card ]
                    ]
            else
                div ( draggableAttribute index )
                    [ cardPlaceholder
                    , div
                        [ class "card-space" ]
                        [ Card.view card ]
                    ]


-- ####
-- ####    UPDATE
-- ####


-- ####
-- ####    HELPER
-- ####


pushCard : Int -> Card -> Model -> Model
pushCard spaceId card model =
    { model
    | spaces = Array.set spaceId ( Just card ) model.spaces
    }


pullCard : Int -> Model -> Model
pullCard spaceId model =
    { model
    | spaces = Array.set spaceId Nothing model.spaces
    }


moveCard : Int -> Int -> Model -> Model
moveCard spaceFromId spaceToId model =
    case Array.get spaceFromId model.spaces of
        Nothing ->
            model

        Just maybeCard ->
            { model
            | spaces =
                Array.set spaceFromId Nothing model.spaces
                |> Array.set spaceToId maybeCard
            }


getCard : Int -> Model -> Maybe Card
getCard spaceId model =
    case Array.get spaceId model.spaces of
        Nothing ->
            Nothing

        Just maybeCard ->
            maybeCard

