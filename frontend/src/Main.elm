port module Main exposing (main)

import Animate
import Array
import Browser exposing (Document, UrlRequest)
import Buttons
import CardsCDN
import Distribute exposing (Msg(..))
import EndAnimation
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
    , animateModel : Animate.Model
    , afterAnimationModel : ModelHistoryMoment
    , endAnimationModel : EndAnimation.Model
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
                , animateModel = Animate.init
                , afterAnimationModel = ModelHistory.initMoment
                , endAnimationModel = EndAnimation.init
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
        hasHistory = ModelHistory.hasHistory model.modelHistory && not ( Pile.playingDone pilesModel )
        helpForPile = helperForPile ( DragDrop.getDragId model.dragDrop ) model
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
                    if ( Home.playingDone homesModel ) then
                        [
                            CardsCDN.stylesheet
                            , Buttons.view helpForButtons
                            , EndAnimation.view model.endAnimationModel
                        ]
                    else
                        [
                            CardsCDN.stylesheet
                            , Buttons.view helpForButtons
                            , Space.view spacesModel helpForSpace
                            , Home.view homesModel helpForHome
                            , Pile.view pilesModel helpForPile
                            , Animate.view model.animateModel
                        ]
        }


helperForPile : Maybe From -> Model -> Pile.Helper Msg
helperForPile maybeFrom model =
    let
        { pilesModel, spacesModel, homesModel } = ModelHistory.getCurrent model.modelHistory
        distributingDone = model.distributeModel.distributingDone
    in
        if not distributingDone then
            {
                maybeDragFromPileId = Nothing
                , maybeDragFromCardId = Nothing
                , maybeDragCard = Nothing
                , draggedNumberOfCards = 0
                , maybeDroppableAttribute = Nothing
                , maybeDraggableAttribute = Nothing
                , emptyPiles = 0
                , emptySpaces = 0
                , maybeClickToSendHome = Nothing
                , cardClass = [ class "card-distributing" ]
            }
        else if Animate.animating model.animateModel then
            {
                maybeDragFromPileId = Nothing
                , maybeDragFromCardId = Nothing
                , maybeDragCard = Nothing
                , draggedNumberOfCards = 0
                , maybeDroppableAttribute = Nothing
                , maybeDraggableAttribute = Nothing
                , emptyPiles = 0
                , emptySpaces = 0
                , maybeClickToSendHome = Nothing
                , cardClass = []
            }
        else
            case maybeFrom of
                Just ( PileFrom pileIndex cardIndex ) ->
                    {
                        maybeDragFromPileId = Just pileIndex
                        , maybeDragFromCardId = Just cardIndex
                        , maybeDragCard = Pile.getCard pileIndex cardIndex pilesModel
                        , draggedNumberOfCards = Pile.getNumberOfCards pileIndex cardIndex pilesModel
                        , maybeDroppableAttribute = Just droppablePiles
                        , maybeDraggableAttribute = Just draggablePiles
                        , emptyPiles = Pile.getEmptyPiles pilesModel
                        , emptySpaces = Space.getEmptySpaces spacesModel
                        , maybeClickToSendHome = Just clickToSendHomeFromPile
                        , cardClass = []
                    }

                Just ( SpaceFrom spaceIndex ) ->
                    {
                        maybeDragFromPileId = Nothing
                        , maybeDragFromCardId = Nothing
                        , maybeDragCard = Space.getCard spaceIndex spacesModel
                        , draggedNumberOfCards = 1
                        , maybeDroppableAttribute = Just droppablePiles
                        , maybeDraggableAttribute = Just draggablePiles
                        , emptyPiles = Pile.getEmptyPiles pilesModel
                        , emptySpaces = Space.getEmptySpaces spacesModel
                        , maybeClickToSendHome = Just clickToSendHomeFromPile
                        , cardClass = []
                    }

                _ ->
                    {
                        maybeDragFromPileId = Nothing
                        , maybeDragFromCardId = Nothing
                        , maybeDragCard = Nothing
                        , draggedNumberOfCards = 0
                        , maybeDroppableAttribute = Just droppablePiles
                        , maybeDraggableAttribute = Just draggablePiles
                        , emptyPiles = Pile.getEmptyPiles pilesModel
                        , emptySpaces = Space.getEmptySpaces spacesModel
                        , maybeClickToSendHome = Just clickToSendHomeFromPile
                        , cardClass = []
                      }


helperForSpace : Maybe From -> Model -> Space.Helper Msg
helperForSpace maybeFrom model =
    let
        { pilesModel, spacesModel, homesModel } = ModelHistory.getCurrent model.modelHistory
    in
        if Animate.animating model.animateModel then
            {
                maybeDragFromSpaceId = Nothing
                , draggedNumberOfCards = 0
                , maybeDroppableAttribute = Nothing
                , maybeDraggableAttribute = Nothing
                , maybeClickToSendHomeFromSpace = Nothing
            }

        else
            case maybeFrom of
                Just ( PileFrom pileIndex cardIndex ) ->
                    {
                        maybeDragFromSpaceId = Nothing
                        , draggedNumberOfCards = Pile.getNumberOfCards pileIndex cardIndex pilesModel
                        , maybeDroppableAttribute = Just droppableSpaces
                        , maybeDraggableAttribute = Just draggableSpaces
                        , maybeClickToSendHomeFromSpace = Just clickToSendHomeFromSpace
                    }

                Just ( SpaceFrom spaceIndex ) ->
                    {
                        maybeDragFromSpaceId = Just spaceIndex
                        , draggedNumberOfCards = 1
                        , maybeDroppableAttribute = Just droppableSpaces
                        , maybeDraggableAttribute = Just draggableSpaces
                        , maybeClickToSendHomeFromSpace = Just clickToSendHomeFromSpace
                    }

                _ ->
                    {
                        maybeDragFromSpaceId = Nothing
                        , draggedNumberOfCards = 0
                        , maybeDroppableAttribute = Just droppableSpaces
                        , maybeDraggableAttribute = Just draggableSpaces
                        , maybeClickToSendHomeFromSpace = Just clickToSendHomeFromSpace
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
    | AnimateMsg Animate.Msg
    | DragDropMsg (DragDrop.Msg From To)
    | SentHomeFromPileMsg Int Card
    | SentHomeFromSpaceMsg Int Card
    | NewMsg
    | RestartMsg
    | UndoMsg
    | EndAnimationMsg EndAnimation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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

        AnimateMsg animateMsg ->
            let
                ( animateModel, animateCmd ) =
                    Animate.update animateMsg model.animateModel
            in
                if Animate.animating animateModel then
                    (
                        { model
                        | animateModel = animateModel
                        }
                        , Cmd.map AnimateMsg animateCmd
                    )
                else
                    let
                        model2 =
                            { model
                            | modelHistory = ModelHistory.setCurrent model.modelHistory model.afterAnimationModel
                            , animateModel = animateModel
                            }
                    in
                        ( model2
                        , possiblyCloseTheGame model2 )

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
                                , possiblyCloseTheGame model2
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
                                        , possiblyCloseTheGame model2
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
                                        , possiblyCloseTheGame model2
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
                    let
                        ( animateModel, animateCmd ) = Animate.start card ( Pile.getCoordinates  ( ModelHistory.getPiles model.modelHistory ) pileIndex ) ( Home.getCoordinates index )
                    in
                        (
                            { model
                            | modelHistory = ModelHistory.getCurrent model.modelHistory
                                |> ModelHistory.setPiles ( Pile.pullCard pileIndex ( ModelHistory.getPiles model.modelHistory ) )
                                -- This one NOT : is flying in animation
                                --|> ModelHistory.setHomes ( Home.pushCard index card ( ModelHistory.getHomes model.modelHistory ) )
                                |> ModelHistory.addMoment model.modelHistory
                            , afterAnimationModel = ModelHistory.getCurrent model.modelHistory
                                |> ModelHistory.setPiles ( Pile.pullCard pileIndex ( ModelHistory.getPiles model.modelHistory ) )
                                |> ModelHistory.setHomes ( Home.pushCard index card ( ModelHistory.getHomes model.modelHistory ) )
                            , animateModel = animateModel
                            }
                            , Cmd.map AnimateMsg animateCmd
                        )

        SentHomeFromSpaceMsg spaceIndex card ->
            case Home.canReceiveCard card ( ModelHistory.getHomes model.modelHistory ) of
                Nothing ->
                    ( model, Cmd.none )

                Just index ->
                    let
                        ( animateModel, animateCmd ) = Animate.start card ( Space.getCoordinates spaceIndex ) ( Home.getCoordinates index )
                    in
                    (
                        { model
                        | modelHistory = ModelHistory.getCurrent model.modelHistory
                            |> ModelHistory.setSpaces ( Space.pullCard spaceIndex ( ModelHistory.getSpaces model.modelHistory ) )
                            -- This one NOT : is flying in animation
                            --|> ModelHistory.setHomes ( Home.pushCard index card ( ModelHistory.getHomes model.modelHistory ) )
                            |> ModelHistory.addMoment model.modelHistory
                        , afterAnimationModel = ModelHistory.getCurrent model.modelHistory
                            |> ModelHistory.setSpaces ( Space.pullCard spaceIndex ( ModelHistory.getSpaces model.modelHistory ) )
                            |> ModelHistory.setHomes ( Home.pushCard index card ( ModelHistory.getHomes model.modelHistory ) )
                        , animateModel = animateModel
                        }
                        , Cmd.map AnimateMsg animateCmd
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
                , endAnimationModel = EndAnimation.init
                }
                , Cmd.none
            )

        NewMsg ->
            (
                init "new"
            )

        EndAnimationMsg endAnimationMsg ->
            let
                ( endAnimationModel, endAnimationCmd ) =
                    EndAnimation.update endAnimationMsg model.endAnimationModel
            in
                (
                    { model
                    | endAnimationModel = endAnimationModel
                    }
                    , Cmd.map EndAnimationMsg endAnimationCmd
                )


possiblyCloseTheGame : Model -> Cmd Msg
possiblyCloseTheGame { modelHistory } =
    if Home.playingDone ( ModelHistory.getHomes modelHistory ) then
        Cmd.map EndAnimationMsg EndAnimation.animate
    else
        if ModelHistory.playingDone modelHistory then
            doSentHomeAll modelHistory
        else
            Cmd.none


doSentHomeAll : ModelHistory -> Cmd Msg
doSentHomeAll modelHistory =
    case modelHistory
        |> getPiles
        |> Pile.getTopCards
        |> Array.map ( trySendMeHome ( ModelHistory.getHomes modelHistory ) )
        |> Array.filter ( \( maybeCard, _ ) -> maybeCard /= Nothing )
        |> Array.toList
    of
        [] ->
            doSentHomeAllFromSpace modelHistory

        ( Just card, pileIndex ) :: _ ->
            Process.sleep 40
                |> Task.perform
                    ( always ( SentHomeFromPileMsg pileIndex card ) )

        ( Nothing, _ ) :: _ ->
            doSentHomeAllFromSpace modelHistory


trySendMeHome : Home.Model -> ( Card, Int ) -> ( Maybe Card, Int )
trySendMeHome homes ( card, index ) =
    case Home.canReceiveCard card homes of
        Nothing ->
            ( Nothing, -1 )

        Just _ ->
            ( Just card, index )


doSentHomeAllFromSpace : ModelHistory -> Cmd Msg
doSentHomeAllFromSpace modelHistory =
    case modelHistory
        |> getSpaces
        |> Space.getCards
        |> Array.map ( trySendMeHome ( ModelHistory.getHomes modelHistory ) )
        |> Array.filter ( \( maybeCard, _ ) -> maybeCard /= Nothing )
        |> Array.toList
    of
        [] ->
            Cmd.none

        ( Just card, homeIndex ) :: _ ->
            Process.sleep 40
                |> Task.perform
                    ( always ( SentHomeFromSpaceMsg homeIndex card ) )

        ( Nothing, _ ) :: _ ->
            Cmd.none
