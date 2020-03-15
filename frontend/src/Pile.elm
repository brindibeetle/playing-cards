module Pile exposing (..)

import Array as Array exposing (..)

import Card exposing (..)
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)
import Html5.DragDrop as DragDrop
import Json.Decode as Json exposing (Value)


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

-- ####
-- ####    DRAGHELPER
-- ####


type alias DragHelper msg =
    { maybeDragFromPileId : Maybe Int, maybeDragCard : Maybe Card, droppableAttribute : Int -> List (Attribute msg), draggableAttribute : ( Int, Int ) -> List (Attribute msg) }


-- ####
-- ####    VIEW
-- ####


view : Model -> DragHelper msg -> Html msg
view model dragHelper =
    div [ class "piles-container" ]
        ( viewPiles model.piles dragHelper )


viewPiles : Array Pile -> DragHelper msg -> List (Html msg)
viewPiles piles dragHelper =
    Array.indexedMap ( viewPile dragHelper ) piles |> Array.toList


viewPile : DragHelper msg -> Int -> Pile -> Html msg
viewPile dragHelper pileIndex pile =
    let
        { maybeDragFromPileId, maybeDragCard, droppableAttribute } = Debug.log "viewPile dragHelper" dragHelper
        draggableFrom = canBeDraggedFrom pile
    in
        case maybeDragCard of
            Just draggedCard ->
                if pileIndex == ( Maybe.withDefault 99 maybeDragFromPileId ) then
                    div ( List.concat [ [ class "pile"], droppableAttribute pileIndex ] )
                        [ cardPlaceholder
                        , viewCardsRecursively dragHelper pileIndex 0 pile draggableFrom
                        ]
                else
                    if cardsSuccessivePile ( getTopCardOfPile pile ) draggedCard then
                        div ( List.concat [ [ class "pile"], droppableAttribute pileIndex ] )
                        [ cardPlaceholder
                        , viewCardsRecursively dragHelper pileIndex 0 pile draggableFrom
                        ]
                    else
                        div ( List.concat [ [ class "pile"] ] )
                            [ cardPlaceholder
                            , viewCardsRecursively dragHelper pileIndex 0 pile draggableFrom
                            ]
            Nothing ->
                div ( List.concat [ [ class "pile"] ] )
                    [ cardPlaceholder
                    , viewCardsRecursively dragHelper pileIndex 0 pile draggableFrom
                    ]





        --( List.concat
        --    [
        --        [ text ( "pile" ++ String.fromInt pileIndex ) ]
        --        , ( Array.indexedMap (\cardIndex card -> viewCardinPile card pileIndex cardIndex) pile ) |> Array.toList
        --    ]
        --)

viewCardsRecursively : DragHelper msg -> Int -> Int -> Array Card -> Int -> Html msg
viewCardsRecursively dragHelper pileIndex cardIndex cards draggableFrom =
    let
        draggableAttributes =
            if cardIndex >= draggableFrom then
                class "card-draggable" ::  draggableAttribute ( pileIndex, cardIndex )
            else
                []
        { draggableAttribute } = dragHelper
    in
        case Array.get cardIndex cards of
            Nothing ->
                div [][]
            Just card ->
                --if cardIndex >= dragCardId then
                --    div [][]
                --else
                    if cardIndex == 0 then
                        div
                            ( class "card card-pile card-pile-top" :: draggableAttributes )
                            [ Card.view card
                            , viewCardsRecursively dragHelper pileIndex ( cardIndex + 1 ) cards draggableFrom
                            ]
                    else
                        div ( class "card card-pile" :: draggableAttributes )
                            [ Card.view card
                            , viewCardsRecursively dragHelper pileIndex ( cardIndex + 1 ) cards draggableFrom
                        ]

--
--viewCardinPile : Card -> Int -> Int -> Html Msg
--viewCardinPile card pileIndex cardIndex =
--    if cardIndex == 0 then
--        div
--            [ class "card cardInPile cardInPileTop" ]
--            [ img ( List.concat [ [ getImage card, class "card"], DragDrop.draggable DragDropMsg ( pileIndex, cardIndex ) ] ) [ ]
--            ]
--    else
--        div [ class "card cardInPile" ]
--            [ img ( List.concat [ [ getImage card, class "card"], DragDrop.draggable DragDropMsg ( pileIndex, cardIndex ) ] ) [ ]
--        --[ img [ getImage card, class "card"] [ ]
--        ]


