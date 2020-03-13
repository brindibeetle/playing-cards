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
         dragId = Debug.log "dragid" ( DragDrop.getDragId model.dragDrop )
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
                , div []
                   ( viewPiles model.cards )
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


viewPiles : Array Pile -> List (Html Msg)
viewPiles piles =
    Array.indexedMap viewPile piles |> Array.toList


viewPile : Int -> Pile -> Html Msg
viewPile pileIndex pile =
    div ( List.concat [ [ class "pile"], DragDrop.droppable DragDropMsg pileIndex ] )
        ( List.concat
            [
                [ text ( "pile" ++ String.fromInt pileIndex ) ]
                , ( Array.indexedMap (\cardIndex card -> viewCardinPile card pileIndex cardIndex) pile ) |> Array.toList
            ]
        )


viewCardinPile : Card -> Int -> Int -> Html Msg
viewCardinPile card pileIndex cardIndex =
    if cardIndex == 0 then
        div
            [ class "card cardInPile cardInPileTop" ]
            [ img ( List.concat [ [ getImage card, class "card"], DragDrop.draggable DragDropMsg ( pileIndex, cardIndex ) ] ) [ ]
            ]
    else
        div [ class "card cardInPile" ]
            [ img ( List.concat [ [ getImage card, class "card"], DragDrop.draggable DragDropMsg ( pileIndex, cardIndex ) ] ) [ ]
        --[ img [ getImage card, class "card"] [ ]
        ]


