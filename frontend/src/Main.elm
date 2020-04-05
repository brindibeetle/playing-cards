port module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Buttons
import CardsCDN
import Distribute exposing (Msg(..))
import Home
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onDoubleClick)
import Html5.DragDrop as DragDrop exposing (..)
import Card exposing (..)
import Json.Decode
import ModelHistory exposing (..)
import Pile exposing (..)
import Process
import Shuffle exposing (..)
import Space
import Task


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


type alias Model =
    { modelHistory : ModelHistory
    , shuffleModel : Shuffle.Model
    , distributeModel : Distribute.Model
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
    in
        (
            {
                modelHistory = ModelHistory.init
                , shuffleModel = shuffleModel
                , distributeModel = Distribute.init
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
        --playingDone = Pile.playingDone model.pilesModel
        --playingDone = Home.playingDone model.homesModel
        hasHistory = ModelHistory.hasHistory model.modelHistory
        distributingDone = model.distributeModel.distributingDone
        helpForPile = helperForPile ( DragDrop.getDragId model.dragDrop ) distributingDone model
        helpForHome = helperForHome ( DragDrop.getDragId model.dragDrop ) model
        helpForSpace = helperForSpace ( DragDrop.getDragId model.dragDrop ) model
        { pilesModel, spacesModel, homesModel } = ModelHistory.getCurrent model.modelHistory
        helpForButtons = { newClicked = NewMsg, restartClicked = RestartMsg, undoClicked = UndoMsg,  newEnabled = True, restartEnabled = hasHistory, undoEnabled = hasHistory }
    in
        { title = "Cards"
        , body =
            if not ( model.shuffleModel.shufflingDone ) then
                [
                    CardsCDN.stylesheet
                    , Buttons.view helpForButtons
                    , Space.view spacesModel helpForSpace
                    , Home.view homesModel helpForHome
                    , Shuffle.view model.shuffleModel
                ]
            else
                if not ( model.distributeModel.distributingDone ) then
                [
                    CardsCDN.stylesheet
                    , Buttons.view helpForButtons
                    , Space.view spacesModel helpForSpace
                    , Home.view homesModel helpForHome
                    , Pile.view pilesModel helpForPile
                ]
                else
                    if ( Pile.playingDone pilesModel ) then
                        [
                            CardsCDN.stylesheet
                            , Buttons.view helpForButtons
                            , Space.view spacesModel helpForSpace
                            , Home.view homesModel helpForHome
                            , Pile.view pilesModel helpForPile
                        ]
                    else
                        [
                            CardsCDN.stylesheet
                            , Buttons.view helpForButtons
                            , Space.view spacesModel helpForSpace
                            , Home.view homesModel helpForHome
                            , Pile.view pilesModel helpForPile
                        ]
        }


helperForPile : Maybe From -> Bool -> Model -> Pile.Helper Msg
helperForPile maybeFrom distributingDone model =
    let
        { pilesModel, spacesModel, homesModel } = ModelHistory.getCurrent model.modelHistory
    in
        case ( distributingDone, maybeFrom ) of
            ( False, _ ) ->
                {
                    maybeDragFromPileId = Nothing
                    , maybeDragFromCardId = Nothing
                    , maybeDragCard = Nothing
                    , draggedNumberOfCards = 0
                    , droppableAttribute = droppablePiles
                    , draggableAttribute = draggablePiles
                    , emptyPiles = 0
                    , emptySpaces = 0
                    , clickToSendHome = clickToSendHomeFromPile
                    , cardClass = [ class "card-distributing" ]
                }

            ( True, Just ( PileFrom pileIndex cardIndex ) ) ->
                {
                    maybeDragFromPileId = Just pileIndex
                    , maybeDragFromCardId = Just cardIndex
                    , maybeDragCard = Pile.getCard pileIndex cardIndex pilesModel
                    , draggedNumberOfCards = Pile.getNumberOfCards pileIndex cardIndex pilesModel
                    , droppableAttribute = droppablePiles
                    , draggableAttribute = draggablePiles
                    , emptyPiles = Debug.log "getEmptyPiles" ( Pile.getEmptyPiles pilesModel )
                    , emptySpaces = Debug.log "getEmptySpaces" ( Space.getEmptySpaces spacesModel )
                    , clickToSendHome = clickToSendHomeFromPile
                    , cardClass = []
                }

            ( True, Just ( SpaceFrom spaceIndex ) ) ->
                {
                    maybeDragFromPileId = Nothing
                    , maybeDragFromCardId = Nothing
                    , maybeDragCard = Space.getCard spaceIndex spacesModel
                    , draggedNumberOfCards = 1
                    , droppableAttribute = droppablePiles
                    , draggableAttribute = draggablePiles
                    , emptyPiles = Debug.log "getEmptyPiles" ( Pile.getEmptyPiles pilesModel )
                    , emptySpaces = Debug.log "getEmptySpaces" ( Space.getEmptySpaces spacesModel )
                    , clickToSendHome = clickToSendHomeFromPile
                    , cardClass = []
                }

            ( True, _ ) ->
                {
                    maybeDragFromPileId = Nothing
                    , maybeDragFromCardId = Nothing
                    , maybeDragCard = Nothing
                    , draggedNumberOfCards = 0
                    , droppableAttribute = droppablePiles
                    , draggableAttribute = draggablePiles
                    , emptyPiles = Debug.log "getEmptyPiles" ( Pile.getEmptyPiles pilesModel )
                    , emptySpaces = Debug.log "getEmptySpaces" ( Space.getEmptySpaces spacesModel )
                    , clickToSendHome = clickToSendHomeFromPile
                    , cardClass = []
                  }


helperForSpace : Maybe From -> Model -> Space.Helper Msg
helperForSpace maybeFrom model =
    let
        { pilesModel, spacesModel, homesModel } = ModelHistory.getCurrent model.modelHistory
    in
        case maybeFrom of
            Just ( PileFrom pileIndex cardIndex ) ->
                {
                    maybeDragFromSpaceId = Nothing
                    , draggedNumberOfCards = Pile.getNumberOfCards pileIndex cardIndex pilesModel
                    , droppableAttribute = droppableSpaces
                    , draggableAttribute = draggableSpaces
                    , clickToSendHomeFromSpace = clickToSendHomeFromSpace
                }

            Just ( SpaceFrom spaceIndex ) ->
                {
                    maybeDragFromSpaceId = Just spaceIndex
                    , draggedNumberOfCards = 1
                    , droppableAttribute = droppableSpaces
                    , draggableAttribute = draggableSpaces
                    , clickToSendHomeFromSpace = clickToSendHomeFromSpace
                }

            _ ->
                {
                    maybeDragFromSpaceId = Nothing
                    , draggedNumberOfCards = 0
                    , droppableAttribute = droppableSpaces
                    , draggableAttribute = draggableSpaces
                    , clickToSendHomeFromSpace = clickToSendHomeFromSpace
                }


helperForHome : Maybe From -> Model -> Home.Helper Msg
helperForHome maybeFrom model =
    let
        { pilesModel, spacesModel, homesModel } = ModelHistory.getCurrent model.modelHistory
    in
        case maybeFrom of
            Just ( PileFrom pileIndex cardIndex ) ->
                {
                    maybeDraggedCard = Pile.getTopCard pileIndex pilesModel
                    , draggedNumberOfCards = Pile.getNumberOfCards pileIndex cardIndex pilesModel
                    , droppableAttribute = droppableHomes
                }

            Just ( SpaceFrom spaceIndex ) ->
                {
                    maybeDraggedCard = Space.getCard spaceIndex spacesModel
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


clickToSendHomeFromPile : Int -> Card -> Attribute Msg
clickToSendHomeFromPile pileIndex card =
    onDoubleClick ( SentHomeFromPileMsg pileIndex card )


clickToSendHomeFromSpace : Int -> Card -> Attribute Msg
clickToSendHomeFromSpace homeIndex card =
    onDoubleClick ( SentHomeFromSpaceMsg homeIndex card )

-- #####
-- #####   UPDATE
-- #####

port dragstart : Json.Decode.Value -> Cmd msg


type Msg
    = ShuffleMsg Shuffle.Msg
    | DistributeMsg Distribute.Msg
    | DragDropMsg (DragDrop.Msg From To)
    | SentHomeFromPileMsg Int Card
    | SentHomeFromSpaceMsg Int Card
    | SentAllHomeFromPileMsg Int
    | SentAllHomeFromSpaceMsg Int
    | NewMsg
    | RestartMsg
    | UndoMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        a = Debug.log "Main.update.msg" msg
    in
        case msg of
            ShuffleMsg shuffleMsg ->
                let
                    ( shuffleModel, shuffleCmd ) =
                        Shuffle.update shuffleMsg model.shuffleModel
                in
                    if shuffleModel.shufflingDone then
                        let
                            ( distributeModel, distributeCmd ) = Distribute.start model.shuffleModel.pile
                        in
                            (
                                { model
                                | shuffleModel = shuffleModel
                                , distributeModel = distributeModel
                                }
                                , Cmd.map DistributeMsg distributeCmd
                            )
                    else
                        (
                            { model
                            | shuffleModel = shuffleModel
                            }
                            , Cmd.map ShuffleMsg shuffleCmd
                        )

            DistributeMsg distributeMsg ->
                let
                    ( distributeModel, distributeCmd ) =
                        Distribute.update distributeMsg model.distributeModel
                in
                    (
                        { model
                        | distributeModel = distributeModel
                        , modelHistory = ModelHistory.getCurrent model.modelHistory
                                |> ModelHistory.setPiles ( Pile.setPiles distributeModel.piles )
                                |> ModelHistory.setCurrent model.modelHistory
                        }
                        , if distributeModel.distributingDone then Cmd.none else Cmd.map DistributeMsg distributeCmd
                    )

            DragDropMsg msg_ ->
                let
                    ( dragDropModel, dragDropResult ) =
                        DragDrop.update msg_ model.dragDrop

                    ( model1, cmd ) =
                        case dragDropResult of
                            Nothing ->
                                (
                                    { model
                                    | dragDrop = dragDropModel
                                    }
                                    , Cmd.none
                                )

                            Just ( PileFrom pileFromId cardFromId, PileTo pileToId, _ ) ->
                                let
                                    model2 =
                                        { model
                                        | dragDrop = dragDropModel
                                        , modelHistory = ModelHistory.getCurrent model.modelHistory
                                                |> ModelHistory.setPiles ( Pile.moveCard pileFromId cardFromId pileToId  ( ModelHistory.getPiles model.modelHistory ) )
                                                |> ModelHistory.addMoment model.modelHistory
                                        }
                                in
                                    ( model2
                                    , if ModelHistory.playingDone model2.modelHistory then doSentHomeAll model2 0  else Cmd.none
                                    )

                            Just ( PileFrom pileFromId _, SpaceTo spaceId, _ ) ->
                                case Pile.getTopCard pileFromId ( ModelHistory.getPiles model.modelHistory ) of
                                    Nothing ->
                                        (
                                            { model
                                            | dragDrop = dragDropModel
                                            }
                                            , Cmd.none
                                        )

                                    Just card ->
                                        let
                                            model2 =
                                                { model
                                                | dragDrop = dragDropModel
                                                , modelHistory = ModelHistory.getCurrent model.modelHistory
                                                        |> ModelHistory.setSpaces ( Space.pushCard spaceId card ( ModelHistory.getSpaces model.modelHistory ) )
                                                        |> ModelHistory.setPiles ( Pile.pullCard pileFromId ( ModelHistory.getPiles model.modelHistory ) )
                                                        |> ModelHistory.addMoment model.modelHistory
                                                }
                                        in
                                            ( model2
                                            , if ModelHistory.playingDone model2.modelHistory then doSentHomeAll model2 0  else Cmd.none
                                            )

                            Just ( SpaceFrom spaceFromId, SpaceTo spaceToId, _ ) ->
                                (
                                    { model
                                    | dragDrop = dragDropModel
                                    , modelHistory = ModelHistory.getCurrent model.modelHistory
                                             |> ModelHistory.setSpaces ( Space.moveCard spaceFromId spaceToId ( ModelHistory.getSpaces model.modelHistory ) )
                                             |> ModelHistory.addMoment model.modelHistory
                                    }
                                    , Cmd.none
                                )

                            Just ( SpaceFrom spaceFromId, PileTo pileToId, _ ) ->
                                case Space.getCard spaceFromId ( ModelHistory.getSpaces model.modelHistory ) of
                                    Nothing ->
                                        (
                                            { model
                                            | dragDrop = dragDropModel
                                            }
                                            , Cmd.none
                                        )

                                    Just card ->
                                        (
                                            { model
                                            | dragDrop = dragDropModel
                                            , modelHistory = ModelHistory.getCurrent model.modelHistory
                                                     |> ModelHistory.setSpaces ( Space.pullCard spaceFromId ( ModelHistory.getSpaces model.modelHistory ) )
                                                     |> ModelHistory.setPiles ( Pile.pushCard pileToId card ( ModelHistory.getPiles model.modelHistory ) )
                                                     |> ModelHistory.addMoment model.modelHistory
                                            }
                                            , Cmd.none
                                        )

                            Just ( PileFrom pileFromId _, HomeTo homeId, _ ) ->
                                case Pile.getTopCard pileFromId ( ModelHistory.getPiles model.modelHistory ) of
                                    Nothing ->
                                        (
                                            { model
                                            | dragDrop = dragDropModel
                                            }
                                            , Cmd.none
                                        )

                                    Just card ->
                                        let
                                            model2 =
                                                { model
                                                | dragDrop = dragDropModel
                                                , modelHistory = ModelHistory.getCurrent model.modelHistory
                                                         |> ModelHistory.setHomes ( Home.pushCard homeId card ( ModelHistory.getHomes model.modelHistory ) )
                                                         |> ModelHistory.setPiles ( Pile.pullCard pileFromId ( ModelHistory.getPiles model.modelHistory ) )
                                                         |> ModelHistory.addMoment model.modelHistory
                                                }
                                        in
                                            ( model2
                                            , if ModelHistory.playingDone model2.modelHistory then doSentHomeAll model2 0  else Cmd.none
                                            )

                            Just ( SpaceFrom spaceFromId, HomeTo homeId, _ ) ->
                                case Space.getCard spaceFromId ( ModelHistory.getSpaces model.modelHistory ) of
                                    Nothing ->
                                        (
                                            { model
                                            | dragDrop = dragDropModel
                                            }
                                            , Cmd.none
                                        )

                                    Just card ->
                                        (
                                            { model
                                            | dragDrop = dragDropModel
                                            , modelHistory = ModelHistory.getCurrent model.modelHistory
                                                     |> ModelHistory.setSpaces ( Space.pullCard spaceFromId ( ModelHistory.getSpaces model.modelHistory ) )
                                                     |> ModelHistory.setHomes ( Home.pushCard homeId card ( ModelHistory.getHomes model.modelHistory ) )
                                                     |> ModelHistory.addMoment model.modelHistory
                                            }
                                            , Cmd.none
                                        )
                in
                    ( model1, Cmd.batch
                            [ cmd
                            , DragDrop.getDragstartEvent msg_
                                |> Maybe.map (.event >> dragstart)
                                |> Maybe.withDefault Cmd.none
                            ]
                    )

            SentHomeFromPileMsg pileIndex card ->
                case Home.canReceiveCard card ( ModelHistory.getHomes model.modelHistory ) of
                    Nothing ->
                        ( model, Cmd.none )

                    Just index ->
                        (
                            { model
                            | modelHistory = ModelHistory.getCurrent model.modelHistory
                                    |> ModelHistory.setPiles ( Pile.pullCard pileIndex ( ModelHistory.getPiles model.modelHistory ) )
                                    |> ModelHistory.setHomes ( Home.pushCard index card ( ModelHistory.getHomes model.modelHistory ) )
                                    |> ModelHistory.addMoment model.modelHistory
                            }
                            , Cmd.none
                        )

            SentHomeFromSpaceMsg homeIndex card ->
                case Home.canReceiveCard card ( ModelHistory.getHomes model.modelHistory ) of
                    Nothing ->
                        ( model, Cmd.none )

                    Just index ->
                        (
                            { model
                            | modelHistory = ModelHistory.getCurrent model.modelHistory
                                    |> ModelHistory.setSpaces ( Space.pullCard homeIndex ( ModelHistory.getSpaces model.modelHistory ) )
                                    |> ModelHistory.setHomes ( Home.pushCard index card ( ModelHistory.getHomes model.modelHistory ) )
                                    |> ModelHistory.addMoment model.modelHistory
                            }
                            , Cmd.none
                        )

            SentAllHomeFromPileMsg pileIndex ->
                case Pile.getTopCard pileIndex ( ModelHistory.getPiles model.modelHistory ) of
                    Nothing ->
                        ( model, doSentHomeAll model ( pileIndex + 1) )

                    Just card ->
                        case Home.canReceiveCard card ( ModelHistory.getHomes model.modelHistory ) of
                            Nothing ->
                                ( model, doSentHomeAll model ( pileIndex + 1) )

                            Just index ->
                                let
                                    model2 =
                                        { model
                                        | modelHistory = ModelHistory.getCurrent model.modelHistory
                                                |> ModelHistory.setPiles ( Pile.pullCard pileIndex ( ModelHistory.getPiles model.modelHistory ) )
                                                |> ModelHistory.setHomes ( Home.pushCard index card ( ModelHistory.getHomes model.modelHistory ) )
                                                |> ModelHistory.setCurrent model.modelHistory
                                        }
                                in
                                    ( model2, doSentHomeAll model2 ( pileIndex + 1)
                                )

            SentAllHomeFromSpaceMsg spaceIndex8 ->
                case Space.getCard ( spaceIndex8 - 8 ) ( ModelHistory.getSpaces model.modelHistory ) of
                    Nothing ->
                        ( model, doSentHomeAll model ( spaceIndex8 + 1) )

                    Just card ->
                        case Home.canReceiveCard card ( ModelHistory.getHomes model.modelHistory ) of
                            Nothing ->
                                ( model, doSentHomeAll model ( spaceIndex8 + 1) )

                            Just index ->
                                let
                                    model2 =
                                        { model
                                        | modelHistory = ModelHistory.getCurrent model.modelHistory
                                                |> ModelHistory.setSpaces ( Space.pullCard ( spaceIndex8 - 8 ) ( ModelHistory.getSpaces model.modelHistory ) )
                                                |> ModelHistory.setHomes ( Home.pushCard index card ( ModelHistory.getHomes model.modelHistory ) )
                                                |> ModelHistory.setCurrent model.modelHistory
                                        }
                                in
                                    ( model2, doSentHomeAll model2 ( spaceIndex8 + 1)
                                )

            UndoMsg ->
                (
                    { model
                    | modelHistory = ModelHistory.popMoment model.modelHistory
                    }
                    , Cmd.none
                )

            RestartMsg ->
                (
                    { model
                    | modelHistory = ModelHistory.popHistory model.modelHistory
                    }
                    , Cmd.none
                )

            NewMsg ->
                (
                    init "new"
                )







doSentHomeAll : Model -> Int -> Cmd Msg
doSentHomeAll model int =
    let
        chooser = modBy 12 int
    in
        if Home.playingDone ( ModelHistory.getHomes model.modelHistory ) then
            Cmd.none
        else
            if chooser < 8 then
                Process.sleep 40
                    |> Task.perform
                        ( always ( SentAllHomeFromPileMsg chooser ) )
            else
                Process.sleep 40
                    |> Task.perform
                        ( always ( SentAllHomeFromSpaceMsg chooser ) )

