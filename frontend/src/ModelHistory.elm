module ModelHistory exposing (..)

import Home
import Pile exposing (emptyAllPiles)
import Space


type alias ModelHistory = List ModelHistoryMoment


type alias ModelHistoryMoment =
    {
        pilesModel : Pile.Model
        , spacesModel : Space.Model
        , homesModel : Home.Model
    }


initMoment : ModelHistoryMoment
initMoment =
    {
        pilesModel = Pile.init
        , spacesModel = Space.init
        , homesModel = Home.init
    }


init : ModelHistory
init = List.singleton initMoment


setCurrent : ModelHistory -> ModelHistoryMoment -> ModelHistory
setCurrent modelHistory modelHistoryMoment =
    modelHistoryMoment :: ( List.drop 1 modelHistory )


getCurrent : ModelHistory -> ModelHistoryMoment
getCurrent modelHistory =
    List.head modelHistory |> Maybe.withDefault initMoment


addMoment : ModelHistory -> ModelHistoryMoment -> ModelHistory
addMoment modelHistory modelHistoryMoment =
    modelHistoryMoment :: modelHistory


popMoment : ModelHistory -> ModelHistory
popMoment modelHistory =
    List.drop 1 modelHistory


popHistory : ModelHistory -> ModelHistory
popHistory modelHistory =
    let
        allButOne = List.length modelHistory - 1
    in
        List.drop allButOne modelHistory


getPiles : ModelHistory -> Pile.Model
getPiles modelHistory =
    modelHistory
        |> getCurrent
        |> .pilesModel


getSpaces : ModelHistory -> Space.Model
getSpaces modelHistory =
    modelHistory
        |> getCurrent
        |> .spacesModel


getHomes : ModelHistory -> Home.Model
getHomes modelHistory =
    modelHistory
        |> getCurrent
        |> .homesModel


setPiles : Pile.Model -> ModelHistoryMoment -> ModelHistoryMoment
setPiles pilesModel modelHistoryCurrent =
    { modelHistoryCurrent
    | pilesModel = pilesModel
    }


setSpaces : Space.Model -> ModelHistoryMoment -> ModelHistoryMoment
setSpaces spacesModel modelHistoryCurrent =
    { modelHistoryCurrent
    | spacesModel = spacesModel
    }


setHomes : Home.Model -> ModelHistoryMoment -> ModelHistoryMoment
setHomes homesModel modelHistoryCurrent =
    { modelHistoryCurrent
    | homesModel = homesModel
    }


playingDone : ModelHistory -> Bool
playingDone modelHistory =
    modelHistory
        |> getCurrent
        |> .pilesModel
        |> Pile.playingDone


allHome : ModelHistory -> Bool
allHome modelHistory =
    modelHistory
        |> getCurrent
        |> .homesModel
        |> Home.playingDone


hasHistory : ModelHistory -> Bool
hasHistory modelHistory =
    List.length modelHistory > 1
