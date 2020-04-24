module Home exposing (..)

import FlyingHome exposing (Coordinates)
import Array exposing (Array)
import Card exposing (Card, cardPlaceholder, cardsSuccessiveHome)
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


type alias Helper msg =
    { maybeDraggedCard : Maybe Card, draggedNumberOfCards : Int, droppableAttribute : Int -> List (Attribute msg) }


-- ####
-- ####    VIEW
-- ####


view : Model -> Helper msg -> Html msg
view model helper =
    div [ class "homes-container" ]
        ( Array.indexedMap (viewHome helper) model.homes |> Array.toList )


viewHome : Helper msg -> Int -> Maybe Card -> Html msg
viewHome helper index maybeCard =
    let
        { maybeDraggedCard, draggedNumberOfCards, droppableAttribute } = helper

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


canReceiveCard : Card -> Model -> Maybe Int
canReceiveCard card { homes } =
    Array.indexedMap
        ( \homeId maybeCard -> (homeId, maybeCard) )
        homes
    |>
    Array.foldl
        ( \(homeId, maybeCard) maybeHomeId ->
            case maybeHomeId of
                Nothing ->
                    if ( cardsSuccessiveHome maybeCard card ) then
                        Just homeId
                    else
                        Nothing

                Just _ ->
                    maybeHomeId
        )
        Nothing


playingDone : Model -> Bool
playingDone model =
    Array.foldl
        ( \maybeCard done ->
            case maybeCard of
                Nothing ->
                    False
                Just card ->
                    done && ( card.rank == Card.King )
        )
        True
        model.homes


getCoordinates : Int -> Coordinates
getCoordinates homeIndex =
    {
        x = 50 + ( homeIndex * 11 )
        , y = 10
    }