module Pile exposing (..)

import Animate exposing (Coordinates)
import Array as Array exposing (..)

import Basics as Math
import Card exposing (..)
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)


--  ####
--  ####      MODEL
--  ####

type alias Model =
    {
        piles : Array Pile
    }


numberOfPiles : Int
numberOfPiles = 8


init : Model
init =
     {
        piles = Array.repeat numberOfPiles Array.empty
     }


setPiles : Array Pile -> Model
setPiles piles =
    { piles = piles
    }

type alias Pile = Array Card


makePile : List Card -> Pile
makePile cards = cards |> Array.fromList


getCard : Int -> Int -> Model -> Maybe Card
getCard pileIndex cardIndex model =
    Array.get pileIndex model.piles |> Maybe.andThen (Array.get cardIndex)


moveCard : Int -> Int -> Int -> Model -> Model
moveCard pileIndexFrom index pileIndexTo model =
    if pileIndexFrom == pileIndexTo then
        model
    else
        case ( Array.get pileIndexFrom model.piles, Array.get pileIndexTo model.piles ) of
            ( Nothing, _ ) ->
                model

            ( _, Nothing ) ->
                model

            ( Just pileFrom, Just pileTo ) ->
                let
                    ( pileFrom1, pileTo1 ) = moveCard2 index ( pileFrom, pileTo )
                in
                    { model
                    | piles = model.piles
                        |> Array.set pileIndexFrom pileFrom1
                        |> Array.set pileIndexTo pileTo1
                    }


moveCard2 : Int -> ( Pile, Pile ) -> ( Pile, Pile )
moveCard2 index ( pileFrom, pileTo ) =
    let
        cards = Array.slice index ( Array.length pileFrom ) pileFrom
    in
        (
            Array.slice 0 index pileFrom
            , Array.append pileTo cards
        )


pullCard : Int -> Model -> Model
pullCard pileIndex model =
    let
        maybePile = Array.get pileIndex model.piles |> Maybe.map ( Array.slice 0 -1 )
    in
        case maybePile of
            Nothing ->
                model

            Just pile ->
                { model
                | piles = Array.set pileIndex pile model.piles
                }

pushCard : Int -> Card -> Model -> Model
pushCard pileIndex card model =
    let
        maybePile = Array.get pileIndex model.piles |> Maybe.map ( Array.push card )
    in
        case maybePile of
            Nothing ->
                model

            Just pile ->
                { model
                | piles = Array.set pileIndex pile model.piles
                }


canBeDraggedFrom : Pile -> Int
canBeDraggedFrom cards =
    let
        lastCard = Array.get ( Array.length cards - 1 ) cards
    in
        case lastCard of
            Nothing ->
                99
            Just card ->
                Array.foldr canBeDraggedFromHelper ( Array.length cards - 1, ( Nothing, True )) cards |> Tuple.first


canBeDraggedFromHelper : Card -> ( Int, ( Maybe Card, Bool )) -> ( Int, ( Maybe Card, Bool ))
canBeDraggedFromHelper card ( cardIndex, ( maybeLastCard, connecting )) =
    case ( maybeLastCard, connecting ) of
        ( _, False ) ->
            ( cardIndex, ( maybeLastCard, False ))

        ( Nothing, True ) ->
            ( cardIndex - 1, ( Just card, True ))

        ( Just lastCard, True ) ->
            if cardsSuccessivePile ( Just card )  lastCard then
                ( cardIndex - 1, ( Just card, True ))
            else
                ( cardIndex + 1, ( Nothing, False ))


getTopCardOfPile : Pile -> Maybe Card
getTopCardOfPile pile  =
    Array.get (Array.length pile - 1) pile


getTopCard : Int -> Model -> Maybe Card
getTopCard pileIndex model  =
    Array.get pileIndex model.piles
    |> Maybe.andThen getTopCardOfPile


getTopCards : Model -> Array ( Card, Int )
getTopCards { piles }  =
    Array.map getTopCardOfPile piles                          -- > Array ( Maybe Card )
    |> Array.indexedMap ( \i maybeCard -> ( maybeCard, i ) )        -- > Array ( Maybe Card, Int)
    |> Array.filter ( \(maybeCard, i ) -> maybeCard /= Nothing )
    |> Array.map ( \(maybeCard, i ) -> ( Maybe.withDefault Card.defaultCard maybeCard, i ) )


getNumberOfCards : Int -> Int -> Model -> Int
getNumberOfCards pileIndex cardIndex model =
    case Array.get pileIndex model.piles of
        Nothing ->
            0

        Just pile ->
            Array.length pile - cardIndex


getEmptyPiles : Model -> Int
getEmptyPiles { piles } =
    Array.foldl
        ( \pile i -> if Array.isEmpty pile then i + 1 else i )
        0
        piles


playingDone : Model -> Bool
playingDone { piles } =
    Array.foldl
        playingDonePile
        True
        piles


playingDonePile : Array Card -> Bool -> Bool
playingDonePile cards okay =
    if not okay then
        False
    else
        Array.foldl
            playingDonePileHelper
            ( True, Nothing )
            cards
        |> Tuple.first


playingDonePileHelper : Card -> ( Bool, Maybe Card ) -> ( Bool, Maybe Card )
playingDonePileHelper card ( okay, maybeLastCard ) =
    case ( okay, maybeLastCard ) of
        ( False, _ ) ->
            ( False, Just card )

        ( True, Nothing ) ->
            ( True, Just card )

        ( True, Just lastCard ) ->
            ( Card.getRank card <= Card.getRank lastCard, Just card )


emptyAllPiles : Model -> Bool
emptyAllPiles { piles } =
    Array.foldl
        (\pile bool -> bool && Array.isEmpty pile)
        True
        piles


-- ####
-- ####    HELPER
-- ####


type alias Helper msg =
    { maybeDragFromPileId : Maybe Int
    , maybeDragFromCardId : Maybe Int
    , maybeDragCard : Maybe Card
    , draggedNumberOfCards : Int
    , maybeDroppableAttribute : Maybe ( Int -> List (Attribute msg) )
    , maybeDraggableAttribute : Maybe ( ( Int, Int ) -> List (Attribute msg) )
    , emptyPiles : Int
    , emptySpaces : Int
    , maybeClickToSendHome : Maybe ( Int -> Card -> Attribute msg )
    , cardClass : List(Attribute msg)
    }


-- ####
-- ####    VIEW
-- ####


view : Model -> Helper msg -> Html msg
view model helper =
    div [ class "piles-container" ]
        ( viewPiles model.piles helper )


viewPiles : Array Pile -> Helper msg -> List (Html msg)
viewPiles piles helper =
    Array.indexedMap ( viewPile helper ) piles |> Array.toList


draggableCards : Int -> Int -> Int
draggableCards emptySpaces emptyPiles =
    if emptyPiles == 0 then
        emptySpaces + 1
    else
        2 * draggableCards emptySpaces ( emptyPiles - 1)


viewPile : Helper msg -> Int -> Pile -> Html msg
viewPile helper pileIndex pile =
    let
        { maybeDragFromPileId, maybeDragFromCardId, maybeDragCard, draggedNumberOfCards, maybeDroppableAttribute, emptyPiles, emptySpaces } = helper
        draggableFrom =  Math.max ( canBeDraggedFrom pile ) ( Array.length pile - ( draggableCards emptySpaces emptyPiles ) )
        droppableAttributeList =
            case maybeDroppableAttribute of
                Nothing ->
                    []
                Just droppableAttribute ->
                    droppableAttribute pileIndex
    in
        case maybeDragCard of
            Just draggedCard ->
                -- cards may go back on pile where they came from :
                if pileIndex == ( Maybe.withDefault 99 maybeDragFromPileId ) then
                    div ( class "pile" :: droppableAttributeList )
                        [ cardPlaceholder
                        , viewCardsRecursively helper pileIndex 0 pile draggableFrom maybeDragFromCardId
                        ]
                else
                    -- if dropping on an empty pile, then you can drop less cards :
                    if Array.isEmpty pile && draggedNumberOfCards > draggableCards emptySpaces ( emptyPiles - 1)  then
                        div ( List.concat [ [ class "pile"] ] )
                        [ cardPlaceholder
                        , viewCardsRecursively helper pileIndex 0 pile draggableFrom Nothing
                        ]
                    else
                        if cardsSuccessivePile ( getTopCardOfPile pile ) draggedCard then
                            div ( class "pile" :: droppableAttributeList )
                            [ cardPlaceholder
                            , viewCardsRecursively helper pileIndex 0 pile draggableFrom Nothing
                            ]
                        else
                            div ( List.concat [ [ class "pile"] ] )
                                [ cardPlaceholder
                                , viewCardsRecursively helper pileIndex 0 pile draggableFrom Nothing
                                ]
            Nothing ->
                div ( List.concat [ [ class "pile"] ] )
                    [ cardPlaceholder
                    , viewCardsRecursively helper pileIndex 0 pile draggableFrom Nothing
                    ]


viewCardsRecursively : Helper msg -> Int -> Int -> Array Card -> Int -> Maybe Int -> Html msg
viewCardsRecursively helper pileIndex cardIndex cards draggableFrom maybeDragFromCardId =
    let
        { maybeDraggableAttribute, maybeClickToSendHome, cardClass } = helper
        draggableAttributes =
            case ( maybeDraggableAttribute, cardIndex >= draggableFrom ) of
                ( Just draggableAttribute, True ) ->
                    class "card-draggable" ::  draggableAttribute ( pileIndex, cardIndex )
                ( _, _ ) ->
                    []
        cardHide =
             if cardIndex >= ( maybeDragFromCardId |> Maybe.withDefault 99 ) then
                 " card-hide"
             else
                 ""
        cardBottom =
              if cardIndex == 0 then
                 " card-pile-bottom"
              else
                 ""
     in
        case Array.get cardIndex cards of
            Nothing ->
                div [][]
            Just card ->
                let
                    onClick =
                            case ( maybeClickToSendHome, cardIndex == Array.length cards - 1 ) of
                                ( Just clickToSendHome, True ) ->
                                    [ clickToSendHome pileIndex card ]
                                ( _, _ ) ->
                                    []
                in
                    div
                        ( List.concat
                            [
                                [ ( class ("card card-pile" ++ cardBottom ++ cardHide)) ]
                                , cardClass
                                , draggableAttributes
                                , onClick
                            ]
                        )
                        [ Card.view card
                        , viewCardsRecursively helper pileIndex ( cardIndex + 1 ) cards draggableFrom maybeDragFromCardId
                        ]



getCoordinates : Model -> Int -> Coordinates
getCoordinates model pileIndex =
    {
        x = 2 + 2 + pileIndex * 12
        , y = 32 + 2 + ( getNumberOfCards pileIndex 0 model ) * 2
    }