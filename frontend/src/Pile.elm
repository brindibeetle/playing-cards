module Pile exposing (..)

import Array as Array exposing (Array)
import Random as Random exposing (..)

import Card exposing (Card, allCards, getImage)


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
    case Array.get index pileFrom of
        Nothing ->
            ( pileFrom, pileTo )

        Just card ->
            (
                 pull1Card index pileFrom
                , push1Card card pileTo
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

