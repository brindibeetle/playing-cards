module Pile exposing (..)

import Array as Array exposing (..)

import Basics as Math
import Card exposing (..)
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onDoubleClick)


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
        piles = Array.empty
     }


fillPiles : Model -> Pile -> Model
fillPiles model pile =
    { model
    | piles = distributeToPiles pile numberOfPiles
    }

type alias Pile = Array Card

emptyPile : Pile
emptyPile = Array.empty


makePile : List Card -> Pile
makePile cards = cards |> Array.fromList


getCard : Int -> Int -> Model -> Maybe Card
getCard pileIndex cardIndex model =
    let
        a = Debug.log "getCard pileIndex" pileIndex
        b = Debug.log "getCard cardIndex" cardIndex
        c = Debug.log "getCard model" model
    in
        Array.get pileIndex model.piles |> Maybe.andThen (Array.get cardIndex)


moveCard : Int -> Int -> Int -> Model -> Model
moveCard pileIndexFrom index pileIndexTo model =
    let
        cards = Debug.log "moveCard pileIndexFrom" ( pileIndexFrom )
    in
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
        cards1 = Debug.log "cardslength" ( Array.length cards )
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

-- ####
-- ####    HELPER
-- ####


type alias Helper msg =
    { maybeDragFromPileId : Maybe Int
    , maybeDragFromCardId : Maybe Int
    , maybeDragCard : Maybe Card
    , draggedNumberOfCards : Int
    , droppableAttribute : Int -> List (Attribute msg)
    , draggableAttribute : ( Int, Int ) -> List (Attribute msg)
    , emptyPiles : Int
    , emptySpaces : Int
    , clickToSendHome : Int -> Card -> Attribute msg
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
        { maybeDragFromPileId, maybeDragFromCardId, maybeDragCard, draggedNumberOfCards, droppableAttribute, emptyPiles, emptySpaces } = Debug.log "viewPile helper" helper
        draggableFrom1 =  Debug.log "canBeDraggedFrom pileIndex" pileIndex
        draggableFrom2 =  Debug.log "canBeDraggedFrom pile" ( canBeDraggedFrom pile )
        a = Debug.log  ("draggableCards emptySpaces = " ++ String.fromInt emptySpaces ++ ", emptyPiles = " ++ String.fromInt emptyPiles ) ( draggableCards emptySpaces emptyPiles )
        draggableFrom =  Math.max ( canBeDraggedFrom pile ) ( Array.length pile - ( draggableCards emptySpaces emptyPiles ) )
    in
        case maybeDragCard of
            Just draggedCard ->
                -- cards may go back on pile where they came from :
                if pileIndex == ( Maybe.withDefault 99 maybeDragFromPileId ) then
                    div ( List.concat [ [ class "pile"], droppableAttribute pileIndex ] )
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
                            div ( List.concat [ [ class "pile"], droppableAttribute pileIndex ] )
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
        { draggableAttribute, clickToSendHome } = helper
        draggableAttributes =
            if cardIndex >= draggableFrom then
                class "card-draggable" ::  draggableAttribute ( pileIndex, cardIndex )
            else
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
                          if cardIndex == Array.length cards - 1 then
                             [ clickToSendHome pileIndex card ]
                          else
                             []
                in
                    div
                        ( List.concat
                            [
                                [ ( class ("card card-pile" ++ cardBottom ++ cardHide)) ]
                                , draggableAttributes
                                , onClick
                            ]
                        )
                        [ Card.view card
                        , viewCardsRecursively helper pileIndex ( cardIndex + 1 ) cards draggableFrom maybeDragFromCardId
                        ]

