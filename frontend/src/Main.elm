port module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import CardsCDN
import Distribute exposing (Msg(..))
import Home
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onDoubleClick)
import Html5.DragDrop as DragDrop exposing (..)
import Card exposing (..)
import Json.Decode
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
    { pilesModel : Pile.Model
    , shuffleModel : Shuffle.Model
    , distributeModel : Distribute.Model
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
    in
        (
            {
                pilesModel = Pile.init
                , shuffleModel = shuffleModel
                , distributeModel = Distribute.init
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
        --playingDone = Pile.playingDone model.pilesModel
        --playingDone = Home.playingDone model.homesModel
        distributingDone = model.distributeModel.distributingDone
        helpForPile = helperForPile ( DragDrop.getDragId model.dragDrop ) distributingDone model
        helpForHome = helperForHome ( DragDrop.getDragId model.dragDrop ) model
        helpForSpace = helperForSpace ( DragDrop.getDragId model.dragDrop ) model
    in
        { title = "Cards"
        , body =
            if not ( model.shuffleModel.shufflingDone ) then
                [
                    CardsCDN.stylesheet
                    , Space.view model.spacesModel helpForSpace
                    , Home.view model.homesModel helpForHome
                    , Shuffle.view model.shuffleModel
                ]
            else
                if not ( model.distributeModel.distributingDone ) then
                [
                    CardsCDN.stylesheet
                    , Space.view model.spacesModel helpForSpace
                    , Home.view model.homesModel helpForHome
                    , Pile.view model.pilesModel helpForPile
                ]
                else
                    if ( Pile.playingDone model.pilesModel ) then
                        [
                            CardsCDN.stylesheet
                            , Space.view model.spacesModel helpForSpace
                            , Home.view model.homesModel helpForHome
                            , Pile.view model.pilesModel helpForPile
                        ]
                    else
                        [
                            CardsCDN.stylesheet
                            , Space.view model.spacesModel helpForSpace
                            , Home.view model.homesModel helpForHome
                            , Pile.view model.pilesModel helpForPile
                        ]
        }


helperForPile : Maybe From -> Bool -> Model -> Pile.Helper Msg
helperForPile maybeFrom distributingDone model =
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
                , maybeDragCard = Pile.getCard pileIndex cardIndex model.pilesModel
                , draggedNumberOfCards = Pile.getNumberOfCards pileIndex cardIndex model.pilesModel
                , droppableAttribute = droppablePiles
                , draggableAttribute = draggablePiles
                , emptyPiles = Debug.log "getEmptyPiles" ( Pile.getEmptyPiles model.pilesModel )
                , emptySpaces = Debug.log "getEmptySpaces" ( Space.getEmptySpaces model.spacesModel )
                , clickToSendHome = clickToSendHomeFromPile
                , cardClass = []
            }

        ( True, Just ( SpaceFrom spaceIndex ) ) ->
            {
                maybeDragFromPileId = Nothing
                , maybeDragFromCardId = Nothing
                , maybeDragCard = Space.getCard spaceIndex model.spacesModel
                , draggedNumberOfCards = 1
                , droppableAttribute = droppablePiles
                , draggableAttribute = draggablePiles
                , emptyPiles = Debug.log "getEmptyPiles" ( Pile.getEmptyPiles model.pilesModel )
                , emptySpaces = Debug.log "getEmptySpaces" ( Space.getEmptySpaces model.spacesModel )
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
                , emptyPiles = Debug.log "getEmptyPiles" ( Pile.getEmptyPiles model.pilesModel )
                , emptySpaces = Debug.log "getEmptySpaces" ( Space.getEmptySpaces model.spacesModel )
                , clickToSendHome = clickToSendHomeFromPile
                , cardClass = []
              }


helperForSpace : Maybe From -> Model -> Space.Helper Msg
helperForSpace maybeFrom model =
    case maybeFrom of
        Just ( PileFrom pileIndex cardIndex ) ->
            {
                maybeDragFromSpaceId = Nothing
                , draggedNumberOfCards = Pile.getNumberOfCards pileIndex cardIndex model.pilesModel
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
                    if distributeModel.distributingDone then
                        (
                            { model
                            | distributeModel = distributeModel
                            , pilesModel = Pile.setPiles model.pilesModel distributeModel.piles
                            }
                            , Cmd.none
                        )
                    else
                        (
                            { model
                            | distributeModel = distributeModel
                            , pilesModel = Pile.setPiles model.pilesModel distributeModel.piles
                            }
                            , Cmd.map DistributeMsg distributeCmd
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
                                        , pilesModel = Pile.moveCard pileFromId cardFromId pileToId model.pilesModel
                                        }
                                in
                                    ( model2
                                    , if Pile.playingDone model2.pilesModel then doSentHomeAll model2 0  else Cmd.none
                                    )

                            Just ( PileFrom pileFromId _, SpaceTo spaceId, _ ) ->
                                case Pile.getTopCard pileFromId model.pilesModel of
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
                                                , spacesModel = Space.pushCard spaceId card model.spacesModel
                                                , pilesModel = Pile.pullCard pileFromId model.pilesModel
                                                }
                                        in
                                            ( model2
                                            , if Pile.playingDone model2.pilesModel then doSentHomeAll model2 0  else Cmd.none
                                            )

                            Just ( SpaceFrom spaceFromId, SpaceTo spaceToId, _ ) ->
                                (
                                    { model
                                    | dragDrop = dragDropModel
                                    , spacesModel = Space.moveCard spaceFromId spaceToId model.spacesModel
                                    }
                                    , Cmd.none
                                )

                            Just ( SpaceFrom spaceFromId, PileTo pileToId, _ ) ->
                                case Space.getCard spaceFromId model.spacesModel of
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
                                            , spacesModel = Space.pullCard spaceFromId model.spacesModel
                                            , pilesModel = Pile.pushCard pileToId card model.pilesModel
                                            }
                                            , Cmd.none
                                        )

                            Just ( PileFrom pileFromId _, HomeTo homeId, _ ) ->
                                case Pile.getTopCard pileFromId model.pilesModel of
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
                                                , homesModel = Home.pushCard homeId card model.homesModel
                                                , pilesModel = Pile.pullCard pileFromId model.pilesModel
                                                }
                                        in
                                            ( model2
                                            , if Pile.playingDone model2.pilesModel then doSentHomeAll model2 0  else Cmd.none
                                            )

                            Just ( SpaceFrom spaceFromId, HomeTo homeId, _ ) ->
                                case Space.getCard spaceFromId model.spacesModel of
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
                                            , spacesModel = Space.pullCard spaceFromId model.spacesModel
                                            , homesModel = Home.pushCard homeId card model.homesModel
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
                case Home.canReceiveCard card model.homesModel of
                    Nothing ->
                        ( model, Cmd.none )

                    Just index ->
                        (
                            { model
                            | pilesModel = Pile.pullCard pileIndex model.pilesModel
                            , homesModel = Home.pushCard index card model.homesModel
                            }
                            , Cmd.none
                        )

            SentHomeFromSpaceMsg homeIndex card ->
                case Home.canReceiveCard card model.homesModel of
                    Nothing ->
                        ( model, Cmd.none )

                    Just index ->
                        (
                            { model
                            | spacesModel = Space.pullCard homeIndex model.spacesModel
                            , homesModel = Home.pushCard index card model.homesModel
                            }
                            , Cmd.none
                        )

            SentAllHomeFromPileMsg pileIndex ->
                case Pile.getTopCard pileIndex model.pilesModel of
                    Nothing ->
                        ( model, doSentHomeAll model ( pileIndex + 1) )

                    Just card ->
                        case Home.canReceiveCard card model.homesModel of
                            Nothing ->
                                ( model, doSentHomeAll model ( pileIndex + 1) )

                            Just index ->
                                let
                                    model2 =
                                        { model
                                        | pilesModel = Pile.pullCard pileIndex model.pilesModel
                                        , homesModel = Home.pushCard index card model.homesModel
                                        }
                                in
                                    ( model2, doSentHomeAll model2 ( pileIndex + 1)
                                )

            SentAllHomeFromSpaceMsg spaceIndex8 ->
                case Space.getCard ( spaceIndex8 - 8 ) model.spacesModel of
                    Nothing ->
                        ( model, doSentHomeAll model ( spaceIndex8 + 1) )

                    Just card ->
                        case Home.canReceiveCard card model.homesModel of
                            Nothing ->
                                ( model, doSentHomeAll model ( spaceIndex8 + 1) )

                            Just index ->
                                let
                                    model2 =
                                        { model
                                        | spacesModel = Space.pullCard ( spaceIndex8 - 8 ) model.spacesModel
                                        , homesModel = Home.pushCard index card model.homesModel
                                        }
                                in
                                    ( model2, doSentHomeAll model2 ( spaceIndex8 + 1)
                                )




doSentHomeAll : Model -> Int -> Cmd Msg
doSentHomeAll model int =
    let
        chooser = modBy 12 int
    in
        if Home.playingDone model.homesModel then
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

