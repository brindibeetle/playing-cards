port module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document, UrlRequest)
import CardsCDN
import Home
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html5.DragDrop as DragDrop exposing (..)
import Card exposing (..)
import Json.Decode
import Pile exposing (..)
import Shuffle exposing (..)
import Space


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


type alias Model =
    { pilesModel : Pile.Model
    , shuffleModel : Shuffle.Model
    , spacesModel : Space.Model
    , homesModel : Home.Model
    , dragDrop : DragDrop.Model From To
    }


type From =
    PileFrom Int Int
    | SpaceFrom Int


type To =
    PileTo Int
    | SpaceTo Int
    | HomeTo Int



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
                pilesModel = Pile.init
                , shuffleModel = shuffleModel
                , spacesModel = Space.init
                , homesModel = Home.init
                , dragDrop = DragDrop.init
            }
            , Cmd.map ShuffleMsg shuffleCmd
        )


-- #####
-- #####   VIEW
-- #####


view : Model -> Document Msg
view model =
    let
        a = Debug.log "getDragId" ( DragDrop.getDragId model.dragDrop )
        dragHelpForPile = Debug.log "dragHelpForPile" ( dragHelperForPile ( DragDrop.getDragId model.dragDrop ) model )
        dragHelpForHome = Debug.log "dragHelpForHome" ( dragHelperForHome ( DragDrop.getDragId model.dragDrop ) model )
        dragHelpForSpace = Debug.log "dragHelpForSpace" ( dragHelperForSpace ( DragDrop.getDragId model.dragDrop ) model )
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
                    , Space.view model.spacesModel dragHelpForSpace
                    , Home.view model.homesModel dragHelpForHome
                    , Pile.view model.pilesModel dragHelpForPile
                ]
        }


dragHelperForPile : Maybe From -> Model -> Pile.DragHelper Msg
dragHelperForPile maybeFrom model =
    case maybeFrom of
        Just ( PileFrom pileIndex cardIndex ) ->
            {
                maybeDragFromPileId = Just pileIndex
                , maybeDragFromCardId = Just cardIndex
                , maybeDragCard = Pile.getCard pileIndex cardIndex model.pilesModel
                , droppableAttribute = droppablePiles
                , draggableAttribute = draggablePiles
            }

        Just ( SpaceFrom spaceIndex ) ->
            {
                maybeDragFromPileId = Nothing
                , maybeDragFromCardId = Nothing
                , maybeDragCard = Space.getCard spaceIndex model.spacesModel
                , droppableAttribute = droppablePiles
                , draggableAttribute = draggablePiles
            }

        _ ->
            {
                maybeDragFromPileId = Nothing
                , maybeDragFromCardId = Nothing
                , maybeDragCard = Nothing
                , droppableAttribute = droppablePiles
                , draggableAttribute = draggablePiles
            }


dragHelperForSpace : Maybe From -> Model -> Space.DragHelper Msg
dragHelperForSpace maybeFrom model =
    case maybeFrom of
        Just ( PileFrom pileIndex cardIndex ) ->
            {
                maybeDragFromSpaceId = Nothing
                , draggedNumberOfCards = Pile.getNumberOfCards pileIndex cardIndex model.pilesModel
                , droppableAttribute = droppableSpaces
                , draggableAttribute = draggableSpaces
            }

        Just ( SpaceFrom spaceIndex ) ->
            {
                maybeDragFromSpaceId = Just spaceIndex
                , draggedNumberOfCards = 1
                , droppableAttribute = droppableSpaces
                , draggableAttribute = draggableSpaces
            }

        _ ->
            {
                maybeDragFromSpaceId = Nothing
                , draggedNumberOfCards = 0
                , droppableAttribute = droppableSpaces
                , draggableAttribute = draggableSpaces
            }


dragHelperForHome : Maybe From -> Model -> Home.DragHelper Msg
dragHelperForHome maybeFrom model =
    case maybeFrom of
        Just ( PileFrom pileIndex cardIndex ) ->
            {
                maybeDraggedCard = Pile.getTopCard pileIndex model.pilesModel
                , draggedNumberOfCards = Pile.getNumberOfCards pileIndex cardIndex model.pilesModel
                , droppableAttribute = droppableHomes
            }

        Just ( SpaceFrom spaceIndex ) ->
            {
                maybeDraggedCard = Space.getCard spaceIndex model.spacesModel
                , draggedNumberOfCards = 1
                , droppableAttribute = droppableHomes
            }

        _ ->
            {
                maybeDraggedCard = Nothing
                , draggedNumberOfCards = 0
                , droppableAttribute = droppableHomes
            }


droppablePiles : Int -> List (Attribute Msg)
droppablePiles pileIndex =
    DragDrop.droppable DragDropMsg (PileTo pileIndex)


draggablePiles : ( Int, Int ) -> List (Attribute Msg)
draggablePiles ( pileIndex, cardIndex ) =
    DragDrop.draggable DragDropMsg (PileFrom pileIndex cardIndex)


droppableSpaces : Int -> List (Attribute Msg)
droppableSpaces spaceIndex =
    DragDrop.droppable DragDropMsg (SpaceTo spaceIndex)


draggableSpaces : Int -> List (Attribute Msg)
draggableSpaces spaceIndex =
    DragDrop.draggable DragDropMsg (SpaceFrom spaceIndex)


droppableHomes : Int -> List (Attribute Msg)
droppableHomes spaceIndex =
    DragDrop.droppable DragDropMsg (HomeTo spaceIndex)



-- #####
-- #####   UPDATE
-- #####

port dragstart : Json.Decode.Value -> Cmd msg


type Msg
    = ShuffleMsg Shuffle.Msg
    | DragDropMsg (DragDrop.Msg From To)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleMsg shuffleMsg ->
            let
                ( shuffleModel, shuffleCmd ) =
                    Shuffle.update shuffleMsg model.shuffleModel
            in
                if shuffleModel.shufflingDone then
                    (
                        { model
                        | shuffleModel = shuffleModel
                        , pilesModel = Pile.fillPiles model.pilesModel shuffleModel.pile
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

        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop

                model12 = Debug.log "model" model_
                result12 = Debug.log "result" result
            in
                (
                    case result of
                        Nothing ->
                            { model
                            | dragDrop = model_
                            }

                        Just ( PileFrom pileFromId cardFromId, PileTo pileToId, _ ) ->
                            { model
                            | dragDrop = model_
                            , pilesModel = Pile.moveCard pileFromId cardFromId pileToId model.pilesModel
                            }

                        Just ( PileFrom pileFromId _, SpaceTo spaceId, _ ) ->
                            case Pile.getTopCard pileFromId model.pilesModel of
                                Nothing ->
                                    { model
                                    | dragDrop = model_
                                    }

                                Just card ->
                                    { model
                                    | dragDrop = model_
                                    , spacesModel = Space.pushCard spaceId card model.spacesModel
                                    , pilesModel = Pile.pullCard pileFromId model.pilesModel
                                    }

                        Just ( SpaceFrom spaceFromId, SpaceTo spaceToId, _ ) ->
                            { model
                            | dragDrop = model_
                            , spacesModel = Space.moveCard spaceFromId spaceToId model.spacesModel
                            }

                        Just ( SpaceFrom spaceFromId, PileTo pileToId, _ ) ->
                            case Space.getCard spaceFromId model.spacesModel of
                                Nothing ->
                                    { model
                                    | dragDrop = model_
                                    }

                                Just card ->
                                    { model
                                    | dragDrop = model_
                                    , spacesModel = Space.pullCard spaceFromId model.spacesModel
                                    , pilesModel = Pile.pushCard pileToId card model.pilesModel
                                    }

                        Just ( PileFrom pileFromId _, HomeTo homeId, _ ) ->
                            case Pile.getTopCard pileFromId model.pilesModel of
                                Nothing ->
                                    { model
                                    | dragDrop = model_
                                    }

                                Just card ->
                                    { model
                                    | dragDrop = model_
                                    , homesModel = Home.pushCard homeId card model.homesModel
                                    , pilesModel = Pile.pullCard pileFromId model.pilesModel
                                    }

                        Just ( SpaceFrom spaceFromId, HomeTo homeId, _ ) ->
                            case Space.getCard spaceFromId model.spacesModel of
                                Nothing ->
                                    { model
                                    | dragDrop = model_
                                    }

                                Just card ->
                                    { model
                                    | dragDrop = model_
                                    , spacesModel = Space.pullCard spaceFromId model.spacesModel
                                    , homesModel = Home.pushCard homeId card model.homesModel
                                    }

                    , DragDrop.getDragstartEvent msg_
                        |> Maybe.map (.event >> dragstart)
                        |> Maybe.withDefault Cmd.none
                )

