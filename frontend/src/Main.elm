port module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document, UrlRequest)
import CardsCDN
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html5.DragDrop as DragDrop exposing (..)
import Json.Decode exposing (Value)
import Card exposing (..)
import Pile exposing (..)
import Shuffle exposing (..)


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }

type alias Model =
    { cards : Array Pile
    , dragDrop : DragDrop.Model ( Int, Int ) Int
    , shuffleModel : Shuffle.Model
    }
    

-- refresh page : 
init : String -> ( Model, Cmd Msg )
init flags =
    let
        ( shuffleModel, shuffleCmd ) =
            Shuffle.init ( makePile allCards )
        shuffleModel1 = Debug.log "shuffleModel" shuffleModel
        shuffleCmd1 = Debug.log "shuffleCmd" shuffleCmd
    in
        (
            {
                cards = Array.empty
                , dragDrop = DragDrop.init
                , shuffleModel = shuffleModel
            }
            , Cmd.map ShuffleMsg shuffleCmd
        )


-- #####
-- #####   VIEW
-- #####


view : Model -> Document Msg
view model =
    let
        maybeDraggedId = Debug.log "maybeDraggedId" ( DragDrop.getDragId model.dragDrop )

        maybeDraggedCard = Maybe.andThen ( \( pileId, cardId) ->  getCardInPile model.cards pileId cardId ) maybeDraggedId
    in
        { title = "Cards"
        , body =
            if not ( model.shuffleModel.shufflingDone ) then
                [
                    CardsCDN.stylesheet
                    , div []
                       [ text "Shuffling "]
                ]
            else
                [
                    CardsCDN.stylesheet
                    , div [ class "piles-container" ]
                       ( viewPiles maybeDraggedId maybeDraggedCard model.cards )
                ]
        }


-- #####
-- #####   UPDATE
-- #####


type Msg
    = DragDropMsg (DragDrop.Msg (Int, Int) Int)
    | ShuffleMsg Shuffle.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop

                model12 = Debug.log "model" model_
                result12 = Debug.log "result" result
            in
                (
                    { model
                    | dragDrop = model_
                    , cards =
                        case result of
                            Nothing ->
                                model.cards

                            Just ( ( pileIndexFrom, cardIndex ) , pileIndexTo , _ ) ->
                                if pileIndexFrom == pileIndexTo then
                                    model.cards
                                else
                                    moveCard pileIndexFrom cardIndex pileIndexTo model.cards

                    }
                    , DragDrop.getDragstartEvent msg_
                        |> Maybe.map (.event >> dragstart)
                        |> Maybe.withDefault Cmd.none
                )

        ShuffleMsg shuffleMsg ->
            let
                ( shuffleModel, shuffleCmd ) =
                    Shuffle.update shuffleMsg model.shuffleModel
            in
                if shuffleModel.shufflingDone then
                    (
                        { model
                        | shuffleModel = shuffleModel
                        , cards = distributeToPiles shuffleModel.pile 8
                        }
                        , Cmd.none
                    )
                else
                    (
                        { model
                        | shuffleModel = shuffleModel
                        }
                        , Cmd.map ShuffleMsg shuffleCmd
                    )


port dragstart : Value -> Cmd msg


viewPiles : Maybe ( Int, Int ) -> Maybe Card -> Array Pile -> List (Html Msg)
viewPiles maybeDraggedId maybeCard piles =
    Array.indexedMap ( viewPile maybeDraggedId maybeCard ) piles |> Array.toList


viewPile : Maybe ( Int, Int ) -> Maybe Card -> Int -> Pile -> Html Msg
viewPile maybeDraggedId maybeDragCard pileIndex pile =
    let
        draggableFrom = canBeDraggedFrom pile
    in
        case ( maybeDraggedId, maybeDragCard ) of
            ( Just ( draggedPileId, draggedCardId ), Just draggedCard ) ->
                if pileIndex == draggedPileId then
                    div ( List.concat [ [ class "pile"], DragDrop.droppable DragDropMsg pileIndex ] )
                        [ cardPlaceholder
                        , viewCardsRecursively pileIndex 0 pile draggedCardId draggableFrom
                        ]
                else
                    if Card.cardsSuccessive ( getTopCardOfPile pile ) draggedCard then
                        div ( List.concat [ [ class "pile"], DragDrop.droppable DragDropMsg pileIndex ] )
                        [ cardPlaceholder
                            , viewCardsRecursively pileIndex 0 pile draggedCardId draggableFrom
                            ]
                    else
                        div ( List.concat [ [ class "pile"] ] )
                            [ cardPlaceholder
                            , viewCardsRecursively pileIndex 0 pile draggedCardId draggableFrom
                            ]
            ( _, _ ) ->
                div ( List.concat [ [ class "pile"] ] )
                    [ cardPlaceholder
                    , viewCardsRecursively pileIndex 0 pile 99 draggableFrom
                    ]





        --( List.concat
        --    [
        --        [ text ( "pile" ++ String.fromInt pileIndex ) ]
        --        , ( Array.indexedMap (\cardIndex card -> viewCardinPile card pileIndex cardIndex) pile ) |> Array.toList
        --    ]
        --)

viewCardsRecursively : Int -> Int -> Array Card -> Int -> Int -> Html Msg
viewCardsRecursively pileIndex cardIndex cards draggedCardId draggableFrom =
    let
        draggableAttributes =
            if cardIndex >= draggableFrom then
                class "card-draggable" :: DragDrop.draggable DragDropMsg ( pileIndex, cardIndex )
            else
                []
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
                            ( class "card cardInPile cardInPileTop" :: draggableAttributes )
                            [ Card.view card
                            , viewCardsRecursively pileIndex ( cardIndex + 1 ) cards draggedCardId draggableFrom
                            ]
                    else
                        div ( class "card cardInPile" :: draggableAttributes )
                            [ Card.view card
                            , viewCardsRecursively pileIndex ( cardIndex + 1 ) cards draggedCardId draggableFrom
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



