module Space exposing (..)

import Animate exposing (Coordinates)
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


getEmptySpaces : Model -> Int
getEmptySpaces { spaces } =
    Array.foldl
        ( \space i -> if space == Nothing then i + 1 else i )
        0
        spaces


-- ####
-- ####    HELPER
-- ####


type alias Helper msg =
    { maybeDragFromSpaceId : Maybe Int
    , draggedNumberOfCards : Int
    , maybeDroppableAttribute : Maybe ( Int -> List (Attribute msg) )
    , maybeDraggableAttribute : Maybe ( Int -> List (Attribute msg) )
    , maybeClickToSendHomeFromSpace : Maybe ( Int -> Card -> Attribute msg )
    }


-- ####
-- ####    VIEW
-- ####


view : Model -> Helper msg -> Html msg
view model helper =
    div [ class "spaces-container" ]
        ( Array.indexedMap (viewSpace helper) model.spaces |> Array.toList )


viewSpace : Helper msg -> Int -> Maybe Card -> Html msg
viewSpace helper index maybeCard =
    let
        { maybeDragFromSpaceId, draggedNumberOfCards, maybeDroppableAttribute, maybeDraggableAttribute, maybeClickToSendHomeFromSpace } = helper
        droppableAttributeList =
            case maybeDroppableAttribute of
                Nothing ->
                    []
                Just droppableAttribute ->
                    droppableAttribute index
        draggableAttributeList =
            case maybeDraggableAttribute of
                Nothing ->
                    []
                Just draggableAttribute ->
                    draggableAttribute index
    in
    case maybeCard  of
        Nothing ->
            if draggedNumberOfCards == 1 then
                div droppableAttributeList
                    [ cardPlaceholder
                    ]
            else
                div []
                    [ cardPlaceholder
                    ]

        Just card ->
            let
                clickToSendHomeFromSpaceList =
                    case maybeClickToSendHomeFromSpace of
                        Nothing ->
                            []
                        Just clickToSendHomeFromSpace ->
                            [ clickToSendHomeFromSpace index card ]
            in
                if ( maybeDragFromSpaceId |> Maybe.withDefault 99 ) == index then
                    div ( List.append draggableAttributeList droppableAttributeList )
                        [ cardPlaceholder
                        , div
                            ( class "card-space card-hide" :: clickToSendHomeFromSpaceList )
                            [ Card.view card ]
                        ]
                else
                    div draggableAttributeList
                        [ cardPlaceholder
                        , div
                            ( class "card-space" :: clickToSendHomeFromSpaceList )
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

getCards : Model -> Array ( Card, Int )
getCards { spaces } =
    spaces
    |> Array.indexedMap ( \i maybeCard -> ( maybeCard, i ) )            -- Array (Maybe Card, Int )
    |> Array.filter ( \(maybeCard, i ) -> maybeCard /= Nothing )
    |> Array.map ( \(maybeCard, i ) -> ( Maybe.withDefault Card.defaultCard maybeCard, i ) )

getCoordinates : Int -> Coordinates
getCoordinates spaceIndex =
    {
        x = 5 + ( spaceIndex * 11 )
        , y = 4
    }


emptyAllSpaces : Model -> Bool
emptyAllSpaces { spaces } =
    Array.foldl
        ( \space bool -> bool && space == Nothing )
        True
        spaces
