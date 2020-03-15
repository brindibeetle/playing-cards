module Pile exposing (..)

import Array as Array exposing (..)

import Card exposing (..)


--  ####
--  ####      PILE
--  ####

type alias Pile = Array Card


emptyPile : Pile
emptyPile = Array.empty


makePile : List Card -> Pile
makePile cards = cards |> Array.fromList


getCard : Int -> Int -> Array Pile -> Maybe Card
getCard pileIndex cardIndex piles =
    Array.get pileIndex piles |> Maybe.andThen (Array.get cardIndex)


moveCard : Int -> Int -> Int -> Array Pile -> Array Pile
moveCard pileIndexFrom index pileIndexTo piles =
    let
        cards = Debug.log "moveCard pileIndexFrom" ( pileIndexFrom )
    in
        case ( Array.get pileIndexFrom piles, Array.get pileIndexTo piles ) of
            ( Nothing, _ ) ->
                piles

            ( _, Nothing ) ->
                piles

            ( Just pileFrom, Just pileTo ) ->
                let
                    ( pileFrom1, pileTo1 ) = moveCard2 index ( pileFrom, pileTo )
                in
                    piles
                    |> Array.set pileIndexFrom pileFrom1
                    |> Array.set pileIndexTo pileTo1


moveCard2 : Int -> ( Pile, Pile ) -> ( Pile, Pile )
moveCard2 index ( pileFrom, pileTo ) =
    let
        cards = Array.slice index ( Array.length pileFrom ) pileFrom
        cards1 = Debug.log "cardslength" ( Array.length cards )
    in
        (
            Array.slice 0 index pileFrom
            , Array.append pileTo cards
        )


pull1Card : Int -> Pile -> Pile
pull1Card index pile =
    Array.append
        ( Array.slice 0 index pile )
        ( Array.slice ( index + 1) (Array.length pile) pile )


push1Card : Card -> Pile -> Pile
push1Card card pile =
    Array.push card pile


distributeToPiles : Pile -> Int -> Array Pile
distributeToPiles pile number =
    Array.foldl
        distributeCardToPile
        ( Array.repeat number Array.empty, 0 )
        pile
    |> Tuple.first


distributeCardToPile : Card -> ( Array Pile, Int ) -> ( Array Pile, Int )
distributeCardToPile card ( piles, pileIndex ) =
    case Array.get pileIndex  piles of
        Nothing ->
            ( piles, modBy ( Array.length piles ) ( pileIndex + 1 ) )

        Just pile ->
            let
                pile1 = Array.push card pile
                pileIndex1 = modBy ( Array.length piles ) ( pileIndex + 1 )
            in
               ( Array.set pileIndex pile1 piles, pileIndex1 )


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
            if Card.cardsSuccessive ( Just card )  lastCard then
                ( cardIndex - 1, ( Just card, True ))
            else
                ( cardIndex + 1, ( Nothing, False ))


getCardInPile : Array Pile -> Int -> Int -> Maybe Card
getCardInPile piles pileId cardId =
    case Array.get pileId piles of
        Nothing ->
            Nothing

        Just pile ->
            Array.get cardId pile


getTopCardOfPile : Pile -> Maybe Card
getTopCardOfPile pile  =
    Array.get (Array.length pile - 1) pile


-- ####
-- ####    View
-- ####

